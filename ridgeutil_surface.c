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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ridgeutil.h"

RutSurface *
rut_surface_new (int rows, int cols) {
  RutSurface *result = malloc (sizeof(RutSurface));
  size_t len = rows * cols * sizeof (float);
  result->rows = rows;
  result->cols = cols;
  result->rowstep = cols;
  result->colstep = 1;
  result->is_view = 0;
  result->data = rut_multiproc_malloc (len);
  return result;
}

RutSurface *
rut_surface_new_like (RutSurface *s) {
  return rut_surface_new (s->rows, s->cols);
}

void
rut_surface_destroy (RutSurface *s)
{
  if (!s) return;
  if (!s->is_view) {
    rut_multiproc_free (s->data);
  }
  free (s);
}

RutSurface *
rut_surface_new_view (RutSurface *s)
{
  assert (s);
  RutSurface *result = malloc (sizeof (RutSurface));
  memcpy (result, s, sizeof (RutSurface));
  result->is_view = 1;
  return result;
}

RutSurface *
rut_surface_new_view_extents (RutSurface *s, RutExtents *extents)
{
  assert (extents);
  assert (extents->top >= 0 && extents->top < s->rows);
  assert (extents->left >= 0 && extents->left < s->cols);
  assert (extents->height > 0 && extents->top + extents->height <= s->rows);
  assert (extents->width > 0 && extents->left + extents->width <= s->cols);

  RutSurface *result = rut_surface_new_view (s);
  /* Set up single row view */
  result->data = RUT_SURFACE_PTR (result, extents->top, extents->left);
  result->rows = extents->height;
  result->cols = extents->width;
  return result;
}

void
rut_surface_transpose (RutSurface *s)
{
  int tmp;
  assert (s);

  tmp = s->rows;
  s->rows = s->cols;
  s->cols = tmp;

  tmp = s->rowstep;
  s->rowstep = s->colstep;
  s->colstep = tmp;
}
