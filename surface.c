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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <tiffio.h>

#include "ridgetool.h"

SurfaceView *
surface_view_new (RutSurface *target)
{
  SurfaceView *result;
  assert (target);

  result = malloc (sizeof (SurfaceView));
  result->len = 0;
  result->ofs = 0;
  result->stride = 0;
  result->target = target;

  return result;
}

void
surface_view_destroy (SurfaceView *view)
{
  assert (view);
  free (view);
}

void
surface_view_set_row (SurfaceView *view, int row)
{
  assert (view);
  assert (view->target);
  assert (row >= 0);
  assert (row < view->target->rows);

  view->len = view->target->cols;
  view->ofs = view->target->cols * row;
  view->stride = 1;
}

void
surface_view_set_col (SurfaceView *view, int col)
{
  assert (view);
  assert (view->target);
  assert (col >= 0);
  assert (col < view->target->cols);

  view->len = view->target->rows;
  view->ofs = col;
  view->stride = view->target->cols;
}
