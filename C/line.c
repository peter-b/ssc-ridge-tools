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
    fprintf (stderr,
             "%s: failed (ptr = %p, oldval = [%lx, %lx], newval = [%lx, %lx])\n",
             __FUNCTION__, (void *)ptr,
             old.s.l, old.s.h, new.s.l, new.s.h);
  }

  return result;
}

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
  assert (sizeof (RidgeLinesSSEntry) == sizeof (__int128_t));
  return __sync_bool_compare_and_swap ((__int128_t *) ptr,
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

static void
adjacent_points (RidgePointsSSEntry *curr,
                 RidgePointsSSEntry **adj1,
                 RidgePointsSSEntry **adj2,
                 int rowstep)
{
  int found = 0;
  RidgePointsSSEntry *adj[2] = {NULL, NULL};
  unsigned char nflags;

  assert (curr);
  assert (adj1);
  assert (adj2);

  /* Don't bother with invalid points. */
  if (count_bits_set (curr->flags) != 2) {
    *adj1 = NULL;
    *adj2 = NULL;
    return;
  }

  if (curr->flags & EDGE_FLAG_NORTH) {
    nflags = (curr - rowstep)->flags;
    if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_SOUTH)
      adj[found++] = curr - rowstep;
  }

  if (curr->flags & EDGE_FLAG_SOUTH) {
    nflags = (curr + rowstep)->flags;
    if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_NORTH)
      adj[found++] = curr + rowstep;
  }

  if (curr->flags & EDGE_FLAG_WEST) {
    nflags = (curr - 1)->flags;
    if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_EAST)
      adj[found++] = curr - 1;
  }

  if (curr->flags & EDGE_FLAG_EAST) {
    nflags = (curr + 1)->flags;
    if (count_bits_set (nflags) == 2  && nflags & EDGE_FLAG_WEST)
      adj[found++] = curr + 1;
  }

  *adj1 = adj[0];
  *adj2 = adj[1];
  return;
}

static RidgePointsSSEntry *
walk_points (RidgePointsSSEntry *curr,
             RidgePointsSSEntry *prev,
             int rowstep)
{
  RidgePointsSSEntry *adj[2] = {NULL, NULL};

  assert (curr);

  adjacent_points (curr, &adj[0], &adj[1], rowstep);

  for (int i = 0; i < 2; i++) {
    if (adj[i] != prev)
      return adj[i];
  }
  assert (0); /* If we get here, something's gone seriously wrong. */
  return NULL; /* Shouldn't ever be reached! */
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
       * to the north if this is the first row of our sector. */

      /* North */
      if (row == first_row && flags & EDGE_FLAG_NORTH) {
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
      RidgePointsSSEntry *rootp = RIDGE_POINTS_SS_PTR (p, row, col);
      RidgeLinesSSEntry *root = RIDGE_LINES_SS_PTR (l, row, col);

      RidgePointsSSEntry *neighbours[2], *startp;
      RidgePointsSSEntry *currp = NULL, *prevp = NULL, *nextp = NULL;

      assert (l->cols == p->cols);

      /* Skip locations which aren't line roots */
      if (set_parent (root) != root) continue;

      /* Find line neighbours of the root node */
      adjacent_points (rootp, &neighbours[0], &neighbours[1], p->cols);

      /* No neighbours, so quit straight away */
      if (neighbours[0] == NULL) {
        list_prev (root) = NULL;
        list_next (root) = NULL;
        continue;
      }

      /* Walk to find an end of the list */
      prevp = rootp;
      currp = neighbours[0];
      while (1) {
        nextp = walk_points (currp, prevp, p->cols);

        /* Have we got a loop? */
        if (nextp == rootp) break;

        /* Have we found an end? */
        if (nextp == NULL) break;

        prevp = currp;
        currp = nextp;
      }
      startp = currp;

      /* Now actually build the list */
      prevp = NULL;
      while (1) {
        nextp = walk_points (currp, prevp, p->cols);

        /* Have we got a loop? */
        if (nextp == startp) {
          nextp = NULL;
        }

        /* FIXME VERY EVIL ASSUMPTIONS */
        RidgeLinesSSEntry *curr = root + (currp - rootp);
        RidgeLinesSSEntry *next = nextp ? root + (nextp - rootp) : NULL;
        RidgeLinesSSEntry *prev = prevp ? root + (prevp - rootp) : NULL;

        list_next (curr) = next;
        list_prev (curr) = prev;

        /* Have we found an end? */
        if (nextp == NULL) break;

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

  MP_task (MP_ridge_lines_SS_build1_func, (void *) (&info));
  MP_task (MP_ridge_lines_SS_build2_func, (void *) (&info));
}

/* ------------------------------------------------------ */

RidgeLinesSS *
ridge_lines_SS_new_for_surface (Surface *s)
{
  RidgeLinesSS *result = malloc (sizeof (RidgeLinesSS));
  size_t len;
  char *d;
  result->rows = s->rows;
  result->cols = s->cols;

  /* The +1 is to allow some extra space for alignment */
  len = (result->rows * result->cols + 1) * sizeof (RidgeLinesSSEntry);
  d = MP_malloc (len);
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
  MP_free (lines->raw_entries);
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
