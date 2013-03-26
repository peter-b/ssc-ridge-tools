/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2011  Peter Brett <p.brett@surrey.ac.uk>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ridgetool.h"

#define list_next(x) ((x)->l.next)
#define list_prev(x) ((x)->l.prev)
#define set_parent(x) ((x)->v.parent)
#define set_rank(x) ((x)->v.rank)

/* ------------------------------------------------------ */

/* We need to provide this, because GCC sucks. */
#if SIZEOF_INTPTR_T == 8
bool
__sync_bool_compare_and_swap_16 (__int128_t *ptr,
                                 __int128_t oldval,
                                 __int128_t newval)
{
  bool result;

  union par128
  {
    __int128_t f;
    struct {
      uint64_t l;
      uint64_t h;
    } s;
  };
  union par128 old;
  union par128 new;
  old.f = oldval;
  new.f = newval;

  __asm__ __volatile__
    (
     "lock cmpxchg16b %1\n\t"
     "setz %0"
     : "=q" (result),
       "+m" (*ptr),
       "+d" (old.s.h),
       "+a" (old.s.l)
     : "c"  (new.s.h),
       "b"  (new.s.l)
     : "cc"
     );

  if (!result) {
    debug_printf ("%s: failed (ptr = %p, oldval = [%lx, %lx], "
                  "newval = [%lx, %lx])\n",
                  __FUNCTION__, (void *)ptr,
                  old.s.l, old.s.h, new.s.l, new.s.h);
  }

  return result;
}
#endif

/* ------------------------------------------------------ */

#define max(a,b) ((b > a) ? b : a)

/* Atomic compare-and-swap for RidgeLinesSSEntry pointers.  If the
 * current value of `*ptr' is `compare', writes `value' into `*ptr'.
 *
 * Warning: This only works on 64-bit platforms with a
 * CMPXCHG16B instruction.
 */
static inline int
set_cas (volatile RidgeLinesSSEntry *ptr,
         RidgeLinesSSEntry compare,
         RidgeLinesSSEntry value)
{
  assert (sizeof (RidgeLinesSSEntry) == sizeof (int2ptr_t));
  return __sync_bool_compare_and_swap ((int2ptr_t *) ptr,
                                       compare.i,
                                       value.i);
}

/* Atomic read of RidgeLinesSSEntry. */
static RidgeLinesSSEntry
set_read (RidgeLinesSSEntry *x)
{
  RidgeLinesSSEntry result;
 redo:
  result = *x;
  if (!set_cas (x, result, result)) goto redo;
  return result;
}

/* Initialise a new root RidgeLinesSSEntry. */
static void
set_init (RidgeLinesSSEntry *x)
{
  RidgeLinesSSEntry xv, nxv;
 retry:
  xv = set_read (x);
  if (set_parent (&xv) != NULL) return;

  set_parent (&nxv) = x;
  set_rank (&nxv) = 0;
  if (!set_cas (x, xv, nxv)) goto retry;
}

/* Find root RidgeLinesSSEntry for any tree node.
 *
 * This should do path compression, eventually.
 */
static RidgeLinesSSEntry *
set_find (RidgeLinesSSEntry *x)
{
  assert (x);
  while (1) {
    RidgeLinesSSEntry *p = set_parent (x);
    if (p == NULL) return NULL;
    if (p == x) return x;
    x = p;
  }
}

/* Take the union of two RidgeLinesSSEntry sets. */
static void
set_union (RidgeLinesSSEntry *x, RidgeLinesSSEntry *y)
{
  RidgeLinesSSEntry xv, yv, nxv, nyv;

  /* Make sure that both sets have been initialised. */
  set_init (x);
  set_init (y);

 retry:
  /* Find root of each set */
  x = set_find (x);
  y = set_find (y);
  if (x == y) return;

  /* Read values of set roots, and verify that they are still the set
   * roots. */
  xv = set_read (x);
  yv = set_read (y);
  if (set_parent (&xv) != x || set_parent (&yv) != y) {
    goto retry;
  }

  /* Ensure that x has lower rank (i.e. y will be the new root).
   * If same rank, use pointer value as a fallback sort */
  if (set_rank (&xv) > set_rank (&yv)
      || (set_rank (&xv) == set_rank (&yv) && x > y)) {
    RidgeLinesSSEntry tv, *t;
    t = x; x = y; y = t; /* Swap pointers */
    tv = xv; xv = yv; yv = tv; /* Swap values */
  }

  nyv = yv;
  set_rank (&nyv) = max (set_rank (&nyv), set_rank (&xv) + 1);
  nxv = xv;
  set_parent (&nxv) = y;

  /* Safely update */
  if (!set_cas (y, yv, nyv)) {
    goto retry;
  }
  if (!set_cas (x, xv, nxv)) {
    goto retry;
  }
}

/* ------------------------------------------------------ */

struct MPRidgeLinesSSInfo
{
  RidgeLinesSS *lines;
  RidgePointsSS *points;
};

struct Point
{
  unsigned int row, col;
};

static int
point_equal (struct Point a, struct Point b)
{
  return (a.row == b.row && a.col == b.col);
}

#define POINT_NULL ((struct Point) {-1, -1})

static int
adjacent_points (RidgePointsSS *p,
                 struct Point curr,
                 struct Point *adj1,
                 struct Point *adj2)
{
  int found = 0;
  struct Point adj[2] = {curr, curr};
  unsigned char cflags;
  unsigned char nflags;

  assert (p);
  assert (adj1);
  assert (adj2);

  assert ((curr.row >= 0) && (curr.row < p->rows));
  assert ((curr.col >= 0) && (curr.col < p->cols));

  cflags = RIDGE_POINTS_SS_PTR (p, curr.row, curr.col)->flags;

  if (count_bits_set (cflags) == 2) {
    if (cflags & EDGE_FLAG_NORTH && curr.row > 0) {
      nflags = RIDGE_POINTS_SS_PTR (p, curr.row - 1, curr.col)->flags;
      if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_SOUTH) {
        adj[found++].row--;
      }
    }

    if (cflags & EDGE_FLAG_SOUTH && curr.row + 1 < p->rows) {
      nflags = RIDGE_POINTS_SS_PTR (p, curr.row + 1, curr.col)->flags;
      if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_NORTH) {
        adj[found++].row++;
      }
    }

    if (cflags & EDGE_FLAG_WEST && curr.col > 0) {
      nflags = RIDGE_POINTS_SS_PTR (p, curr.row, curr.col - 1)->flags;
      if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_EAST) {
        adj[found++].col--;
      }
    }

    if (cflags & EDGE_FLAG_EAST && curr.col + 1 < p->cols) {
      nflags = RIDGE_POINTS_SS_PTR (p, curr.row, curr.col + 1)->flags;
      if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_WEST) {
        adj[found++].col++;
      }
    }
  }

  for (int i = found; i < 2; i++) {
    adj[i] = POINT_NULL;
  }

  *adj1 = adj[0];
  *adj2 = adj[1];

  return found;
}

static struct Point
walk_points (RidgePointsSS *p,
             struct Point curr,
             struct Point prev)
{
  struct Point adj[2] = {POINT_NULL, POINT_NULL};

  assert (p);

  adjacent_points (p, curr, &adj[0], &adj[1]);

  for (int i = 0; i < 2; i++) {
    if (!point_equal (adj[i], prev)) {
      return adj[i];
    }
  }
  abort (); /* If we get here, something's gone seriously wrong. */
  return POINT_NULL; /* Shouldn't ever be reached! */
}

/* Line-building, first pass.  Associate each ridge segment in the
 * image with precisely one line root element. */
static void
MP_ridge_lines_SS_build1_func (int thread_num, int thread_count, void *user_data)
{
  struct MPRidgeLinesSSInfo *info = (struct MPRidgeLinesSSInfo *) user_data;
  RidgeLinesSS *l = info->lines;
  RidgePointsSS *p = info->points;
  int first_row, num_rows, row, col;

  first_row = thread_num * (l->rows / thread_count);
  num_rows = (thread_num + 1) * (l->rows / thread_count) - first_row;

  for (row = first_row; row < first_row + num_rows; row++) {
    for (col = 0; col < l->cols; col++) {
      RidgePointsSSEntry *s = RIDGE_POINTS_SS_PTR (p, row, col);
      RidgeLinesSSEntry *x = RIDGE_LINES_SS_PTR (l, row, col);
      unsigned char nflags, flags = s->flags;

      /* Skip locations which don't contain ridge segments */
      if (count_bits_set (flags) != 2) {
        set_parent (x) = NULL;
        continue;
      }

      /* Check if x has any neighbours, and if so, do the required set
       * union operation.  Behold, seriously evil pointer arithmetic!
       *
       * We never need to check to the west; either it's the location
       * we just visited, or it doesn't exist.  We only need to check
       * to the north if this is the first row of our sector but not
       * the first row of the image.. */

      /* North */
      if (first_row > 0 && row == first_row && flags & EDGE_FLAG_NORTH) {
        nflags = (s - p->cols)->flags;
        if (count_bits_set (nflags) == 2 && nflags & EDGE_FLAG_SOUTH)
          set_union (x, x - l->cols);
      }

      /* East */
      if (flags & EDGE_FLAG_EAST) {
        nflags = (s + 1)->flags;
        if (count_bits_set (nflags) == 2 && nflags & EDGE_FLAG_WEST)
          set_union (x, x + 1);
      }

      /* South */
      if (flags & EDGE_FLAG_SOUTH) {
        nflags = (s + p->cols)->flags;
        if (count_bits_set (nflags) == 2 && nflags & EDGE_FLAG_NORTH)
          set_union (x, x + l->cols);
      }
    }
  }
}

/* Line-building, second pass.  Compile ridge segments into
 * doubly-linked lists.  Each process only deals with lines where the
 * root segment is in its own region, but is permitted to follow those
 * lines anywhere. */
static void
MP_ridge_lines_SS_build2_func (int thread_num, int thread_count, void *user_data)
{
  struct MPRidgeLinesSSInfo *info = (struct MPRidgeLinesSSInfo *) user_data;
  RidgeLinesSS *l = info->lines;
  RidgePointsSS *p = info->points;
  int first_row, num_rows, row, col;

  first_row = thread_num * (l->rows / thread_count);
  num_rows = (thread_num + 1) * (l->rows / thread_count) - first_row;

  for (row = first_row; row < first_row + num_rows; row++) {
    for (col = 0; col < l->cols; col++) {
      struct Point rootp = {row, col};
      RidgeLinesSSEntry *root = RIDGE_LINES_SS_PTR (l, row, col);

      int n_neighbours;
      struct Point neighbours[2], startp, endp;
      struct Point currp = POINT_NULL;
      struct Point prevp = POINT_NULL;
      struct Point nextp = POINT_NULL;

      assert (l->cols == p->cols);

      /* Skip locations which aren't line roots */
      if (set_parent (root) != root) continue;

      /* Find line neighbours of the root node */
      n_neighbours = adjacent_points (p, rootp, &neighbours[0], &neighbours[1]);

      /* No neighbours, so quit straight away */
      if (!n_neighbours) {
        list_prev (root) = NULL;
        list_next (root) = NULL;
        continue;
      }

      /* Walk to find an end of the list. We now know this line contains
       * at least 2 ridge segments. */
      prevp = rootp;
      currp = neighbours[0];
      while (1) {
        nextp = walk_points (p, currp, prevp);

        /* Have we got a loop? */
        if (point_equal (nextp, rootp)) {
          break;
        }

        /* Have we found an end? */
        if (point_equal (nextp, POINT_NULL)) {
          break;
        }

        prevp = currp;
        currp = nextp;
      }
      startp = currp;

      /* Now walk to find the other end of the list. We need to find
       * both ends so that we can make sure to link the list in
       * canonical order. */
      if (n_neighbours == 1) {
        /* If root had only one neighbour, it's the end. */
        endp = rootp;
      } else if (point_equal (startp, neighbours[1])) {
        /* If root has start as other neighbour, there's a loop, and
         * root is the end. */
        endp = rootp;
      } else {
        prevp = rootp;
        currp = neighbours[1];
        while (1) {
          nextp = walk_points (p, currp, prevp);
          if (point_equal (nextp, rootp)) abort(); /* We shouldn't find a loop! */
          if (point_equal (nextp, POINT_NULL)) break;

          prevp = currp;
          currp = nextp;
        }
        endp = currp;
      }

      /* If the end is closer to the start of the image than the
       * start, swap end and start. */
      if ((endp.row < startp.row) || (endp.row == startp.row && endp.col < startp.col)) {
        currp = endp;
        endp = startp;
        startp = currp;
      }

      /* Now actually build the list */
      currp = startp;
      prevp = POINT_NULL;
      while (1) {
        nextp = walk_points (p, currp, prevp);

        /* Have we got a loop? */
        if (point_equal (nextp, startp)) {
          nextp = POINT_NULL;
        }

        RidgeLinesSSEntry *curr = RIDGE_LINES_SS_PTR (l, currp.row, currp.col);
        RidgeLinesSSEntry *next, *prev;
        if (point_equal (nextp, POINT_NULL)) {
          next = NULL;
        } else {
          next = RIDGE_LINES_SS_PTR (l, nextp.row, nextp.col);
        }
        if (point_equal (prevp, POINT_NULL)) {
          prev = NULL;
        } else {
          prev = RIDGE_LINES_SS_PTR (l, prevp.row, prevp.col);
        }

        assert (curr != next);
        assert (curr != prev);

        list_next (curr) = next;
        list_prev (curr) = prev;

        /* Have we found an end? */
        if (next == NULL) {
          break;
        }

        prevp = currp;
        currp = nextp;
      }
    }
  }
}

void
MP_ridge_lines_SS_build (RidgeLinesSS *lines, RidgePointsSS *points)
{
  struct MPRidgeLinesSSInfo info;
  info.lines = lines;
  info.points = points;

  debug_printf ("Line pass #1: building sets\n");
  rut_multiproc_task (MP_ridge_lines_SS_build1_func, (void *) (&info));
  debug_printf ("Line pass #2: building lists\n");
  rut_multiproc_task (MP_ridge_lines_SS_build2_func, (void *) (&info));
}

/* ------------------------------------------------------ */

RidgeLinesSS *
ridge_lines_SS_new_for_surface (RutSurface *s)
{
  RidgeLinesSS *result = malloc (sizeof (RidgeLinesSS));
  size_t len;
  char *d;
  result->rows = s->rows;
  result->cols = s->cols;

  /* The +1 is to allow some extra space for alignment */
  len = (result->rows * result->cols + 1) * sizeof (RidgeLinesSSEntry);
  d = rut_multiproc_malloc (len);
  memset (d, 0, len);

  /* Make sure entries pointer is properly aligned */
  result->raw_entries = d;
  result->entries =
    (RidgeLinesSSEntry *) (d + ((intptr_t) d) % sizeof (RidgeLinesSSEntry));

  assert (((intptr_t) result->entries) % sizeof(RidgeLinesSSEntry) == 0);
  return result;
}

void
ridge_lines_SS_destroy (RidgeLinesSS *lines)
{
  if (lines == NULL) return;
  rut_multiproc_free (lines->raw_entries);
  free (lines);
}

void
ridge_lines_SS_entry_get_position (RidgeLinesSS *lines,
                                   RidgeLinesSSEntry *entry,
                                   int *row, int *col)
{
  intptr_t ofs;

  assert (lines);
  assert (entry);
  assert (row);
  assert (col);

  ofs = (intptr_t) (entry - lines->entries);

  *row = ofs / lines->cols;
  *col = ofs % lines->cols;

  assert (*row < lines->rows && *row >= 0);
}

RidgeLinesSSEntry *
ridge_lines_SS_entry_next (RidgeLinesSSEntry *entry)
{
  return list_next (entry);
}

RidgeLinesSSEntry *
ridge_lines_SS_entry_prev (RidgeLinesSSEntry *entry)
{
  return list_prev (entry);
}
