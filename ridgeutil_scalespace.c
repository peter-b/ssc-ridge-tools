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
#include <math.h>
#include <string.h>
#include <assert.h>

#include "ridgeutil.h"

/* Required when sorting scales with qsort(). */
static int
scale_cmp (const void *a, const void *b)
{
  float x = *(const float *) a;
  float y = *(const float *) b;
  if (x == y) return 0;
  if (x < y) return -1;
  return 1;
}

/* Round scales to nearest 1/3.  This is necessary when using the Lim
 * & Stiehl DSS formulation. */
static void
round_scales (int n_scales, float *scales)
{
  for (int i = 0; i < n_scales; i++) {
    scales[i] = rintf (scales[i] * 3) / 3;
  }
}

/* Sort scales, removing duplicate values */
static void
sort_unique_scales (int *n_scales, float *scales)
{
  /* Sort scales */
  qsort (scales, *n_scales, sizeof (float), scale_cmp);

  /* Remove duplicates */
  int c, i;
  for (c = 1, i = 0; c < *n_scales; c++) {
    if (scales[i] == scales[c]) continue;
    scales[++i] = scales[c];
  }
  *n_scales -= (c-i-1);
}

static RutScaleSpace *
rut_scale_space_new (int rows, int cols, int n_scales, const float *scales)
{
  size_t len;
  RutScaleSpace *result;

  assert (scales);
  assert (n_scales > 0);

  result = malloc (sizeof (RutScaleSpace));
  result->rows = rows;
  result->cols = cols;
  result->n_scales = n_scales;
  result->scalestep = rows * cols;
  result->rowstep = cols;
  result->colstep = 1;
  result->is_view = 0;

  len = n_scales * sizeof (float);
  result->scales = malloc (len);
  memcpy (result->scales, scales, len);

  len = rows * cols * n_scales * sizeof (float);
  result->data = rut_multiproc_malloc (len);
  return result;
}

void
rut_scale_space_destroy (RutScaleSpace *s)
{
  if (!s) return;
  if (!s->is_view) {
    free (s->scales);
    rut_multiproc_free (s->data);
  }
  free (s);
}

RutScaleSpace *
rut_scale_space_new_like (RutScaleSpace *s)
{
  assert (s);
  return rut_scale_space_new (s->rows, s->cols, s->n_scales, s->scales);
}

RutScaleSpace *
rut_scale_space_new_view (RutScaleSpace *s)
{
  assert (s);
  RutScaleSpace *result = malloc (sizeof (RutScaleSpace));
  memcpy (s, result, sizeof (RutScaleSpace));
  result->is_view = 1;
  return result;
}

RutScaleSpace *
rut_scale_space_new_view_extents (RutScaleSpace *s, RutExtents *extents)
{
  RutScaleSpace *result = rut_scale_space_new_view (s);

  assert (extents);
  assert (extents->top >= 0 && extents->top < s->rows);
  assert (extents->left >= 0 && extents->left < s->cols);
  assert (extents->scale >= 0 && extents->scale < s->n_scales);
  assert (extents->height > 0 && extents->top + extents->height <= s->cols);
  assert (extents->width > 0 && extents->left + extents->width <= s->cols);
  assert (extents->n_scales > 0 &&
          extents->scale + extents->n_scales <= s->n_scales);

  result->data = RUT_SCALE_SPACE_PTR (result, extents->top, extents->left,
                                      extents->scale);
  result->scales = result->scales + extents->scale;
  result->rows = extents->height;
  result->cols = extents->width;
  result->n_scales = extents->n_scales;
  return result;
}

RutSurface *
rut_scale_space_get_surface (RutScaleSpace *s, int axis, int offset)
{
  RutSurface *result = malloc (sizeof (RutSurface));
  result->is_view = 1;

  switch (axis) {
  case RUT_SCALE_SPACE_ROWS:
    assert (offset >= 0 && offset < s->rows);
    result->rows = s->cols;
    result->rowstep = s->colstep;
    result->cols = s->n_scales;
    result->colstep = s->scalestep;
    result->data = RUT_SCALE_SPACE_PTR (s, offset, 0, 0);
    break;
  case RUT_SCALE_SPACE_COLS:
    assert (offset >= 0 && offset < s->cols);
    result->rows = s->rows;
    result->rowstep = s->rowstep;
    result->cols = s->n_scales;
    result->colstep = s->scalestep;
    result->data = RUT_SCALE_SPACE_PTR (s, 0, offset, 0);
    break;
  case RUT_SCALE_SPACE_SCALE:
    assert (offset >= 0 && offset < s->n_scales);
    result->rows = s->rows;
    result->rowstep = s->rowstep;
    result->cols = s->cols;
    result->colstep = s->colstep;
    result->data = RUT_SCALE_SPACE_PTR (s, 0, 0, offset);
    break;
  default:
    abort ();
  }

  return result;
}

RutScaleSpace *
rut_scale_space_generate_mp (RutSurface *image, int n_scales,
                             const float *scales)
{
  RutScaleSpace *result = NULL;
  RutFilter *filt = NULL;
  RutSurface *src = NULL, *dest = NULL;

  assert (image);
  assert (scales);
  assert (n_scales > 0);

  result = rut_scale_space_new (image->rows, image->cols, n_scales, scales);

  #ifndef PRECISE_GAUSSIAN
  round_scales (result->n_scales, result->scales);
  #endif
  sort_unique_scales (&result->n_scales, result->scales);

#ifndef PRECISE_GAUSSIAN

  /* Special case first scale */
  filt = rut_filter_new_gaussian (result->scales[0]);
  dest = rut_scale_space_get_surface (result, RUT_SCALE_SPACE_SCALE, 0);
  rut_filter_surface_mp (filt, image, dest, RUT_FILTER_ROWS | RUT_FILTER_COLS);
  rut_filter_destroy (filt);

  /* Progressively generate larger scales */
  src = dest;
  for (int i = 1; i < result->n_scales; i++) {
    filt = rut_filter_new_gaussian (result->scales[i] - result->scales[i-1]);
    dest = rut_scale_space_get_surface (result, RUT_SCALE_SPACE_SCALE, 0);
    rut_filter_surface_mp (filt, src, dest, RUT_FILTER_ROWS | RUT_FILTER_COLS);
    rut_filter_destroy (filt);

    /* Rotate surfaces */
    rut_surface_destroy (src);
    src = dest;
  }
  rut_surface_destroy (src);

#else

  for (int i = 0; i < result->n_scales; i++) {
    filt = rut_filter_new_gaussian (result->scales[i]);
    dest = rut_scale_space_get_surface (result, RUT_SCALE_SPACE_SCALE, i);

    rut_filter_surface_mp (filt, image, dest, RUT_FILTER_ROWS | RUT_FILTER_COLS);

    rut_filter_destroy (filt);
    rut_surface_destroy (dest);
  }

#endif

  return result;
}
