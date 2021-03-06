/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2010-2011 Peter Brett <p.brett@surrey.ac.uk>
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

#include <unistd.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ridgetool.h"

static inline float
test_edge_SS (RutSurface *Lp, RutSurface *Lpp,
              int row, int col, int drow, int dcol)
{
  float lp1 = RUT_SURFACE_REF (Lp, row, col);
  float lp2 = RUT_SURFACE_REF (Lp, row + drow, col + dcol);
  float x, lpp1, lpp2;

  /* Ensure that there is a zero-crossing of Lp on the edge */
  if (isnan (lp1) || isnan (lp2)) return -1;
  if ((lp1 > 0) ^ (lp2 <= 0)) return -1;

  /* Calculate position along edge */
  x = lp1 / (lp1 - lp2);

  assert (x >= 0);
  assert (x <= 1);

  /* Interpolate value of Lpp */
  lpp1 = RUT_SURFACE_REF (Lpp, row, col);
  lpp2 = RUT_SURFACE_REF (Lpp, row + drow, col + dcol);
  if (LINTERP (x, lpp1, lpp2) > 0) return -1;
  return x;
}

RidgePointsSS *
ridge_points_SS_new_for_surface (RutSurface *s)
{
  RidgePointsSS *result = malloc (sizeof (RidgePointsSS));
  size_t len = sizeof (RidgePointsSSEntry) * s->rows * s->cols;
  result->rows = s->rows;
  result->cols = s->cols;
  result->entries = rut_multiproc_malloc (len);
  memset (result->entries, 0, len);
  return result;
}

void
ridge_points_SS_entry_get_position (RidgePointsSS *ridges,
                                    RidgePointsSSEntry *entry,
                                    int *row, int *col)
{
  intptr_t ofs;

  assert (ridges);
  assert (entry);
  assert (row);
  assert (col);

  ofs = (intptr_t) (entry - ridges->entries);

  *row = ofs / ridges->cols;
  *col = ofs % ridges->cols;

  assert (*row < ridges->rows && *row >= 0);
}

void
ridge_points_SS_destroy (RidgePointsSS *r)
{
  if (!r) return;
  rut_multiproc_free (r->entries);
  free (r);
}

struct MPRidgePointsSSInfo
{
  RidgePointsSS *ridges;
  RutSurface *Lp, *Lpp;
};

static void
MP_ridge_points_SS_create_func (int thread_num, int thread_count, void *user_data)
{
  struct MPRidgePointsSSInfo *info = (struct MPRidgePointsSSInfo *) user_data;
  int first_row, num_rows, row, col;
  unsigned char *prev_row = malloc (info->ridges->cols);

  first_row = thread_num * (info->Lp->rows / thread_count);
  num_rows = (thread_num + 1) * (info->Lp->rows / thread_count) - first_row;

  /* First populate previous row buffer */
  for (col = 0; col < info->ridges->cols - 1; col++) {
    float v = test_edge_SS (info->Lp, info->Lpp, first_row, col, 0, 1);
    prev_row[col] = (v < 0) ? 255 : rintf (v * 128);
  }

  /* Now find ridges */
  for (row = first_row; row < first_row + num_rows; row++) {
    unsigned char prev_col = 255;
    if (row + 1 < info->Lp->rows) {
      float v = test_edge_SS (info->Lp, info->Lpp, row, 0, 1, 0);
      prev_col = (v < 0) ? 255 : rintf (v * 128);
    }

    for (col = 0; col < info->ridges->cols; col++) {
      RidgePointsSSEntry *entry = RIDGE_POINTS_SS_PTR (info->ridges, row, col);

      /* North */
      if (prev_row[col] != 255) {
        entry->flags |= EDGE_FLAG_NORTH;
      }
      entry->north = prev_row[col];

      /* West */
      if (prev_col != 255) {
        entry->flags |= EDGE_FLAG_WEST;
      }
      entry->west = prev_col;


      prev_row[col] = 255;
      prev_col = 255;
      if ((row + 1 < info->Lp->rows) && (col + 1 < info->Lp->cols)) {
        float v;

        /* South */
        v = test_edge_SS (info->Lp, info->Lpp, row+1, col, 0, 1);
        if (v >= 0) {
          entry->flags |= EDGE_FLAG_SOUTH;
          prev_row[col] = rintf (v * 128);
        }

        /* East */
        v = test_edge_SS (info->Lp, info->Lpp, row, col+1, 1, 0);
        if (v >= 0) {
          entry->flags |= EDGE_FLAG_EAST;
          prev_col = rintf (v * 128);
        }
      }
    }
  }

  free (prev_row);
}

void
MP_ridge_points_SS (RidgePointsSS *ridges, RutSurface *Lp, RutSurface *Lpp)
{
  struct MPRidgePointsSSInfo *info;

  assert (ridges);
  assert (Lp);
  assert (Lpp);

  info = malloc (sizeof (struct MPRidgePointsSSInfo));
  info->ridges = ridges;
  info->Lp = Lp;
  info->Lpp = Lpp;

  rut_multiproc_task (MP_ridge_points_SS_create_func, (void *) info);

  free (info);
}

