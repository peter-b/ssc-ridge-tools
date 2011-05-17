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

#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ridgeutil.h"

/* -------------------------------------------------------------------------- */

/* A SurfaceView provides a vector-like "view" of a single row or
 * single column of a RutSurface. This is to facilitate separable filtering. */

typedef struct _SurfaceView SurfaceView;

struct _SurfaceView {
  int len;
  int ofs;
  int stride;
  RutSurface *target;
};

/* Creates a new surface view backed by target. The surface view is
 * otherwise uninitialised. */
static SurfaceView *
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

/* Destroys a surface view. */
static void
surface_view_destroy (SurfaceView *view)
{
  if (view) free (view);
}

/* Set view to a particular row of the target RutSurface. */
static void surface_view_set_row (SurfaceView *view, int row)
{
  assert (view);
  assert (view->target);
  assert (row >= 0);
  assert (row < view->target->rows);

  view->len = view->target->cols;
  view->ofs = view->target->cols * row;
  view->stride = 1;
}

/* Set view to a particular column of the target RutSurface. */
static void surface_view_set_col (SurfaceView *view, int col) {
  assert (view);
  assert (view->target);
  assert (col >= 0);
  assert (col < view->target->cols);

  view->len = view->target->rows;
  view->ofs = col;
  view->stride = view->target->cols;
}

/* -------------------------------------------------------------------------- */

RutFilter *
rut_filter_new (int len) {
  RutFilter *result = malloc (sizeof(RutFilter));
  result->len = len;
  result->ofs = 0;
  result->data = malloc (len * sizeof (float));
  return result;
}

void
rut_filter_destroy (RutFilter *f) {
  if (!f) return;
  free (f->data);
  free (f);
}

RutFilter *
rut_filter_new_gaussian (float variance)
{
  int N = lrintf (3 * variance);
  int len;
  int i, j;
  RutFilter *f;
  float *buffer, *tmp;

  if (N < 1) {
    return NULL;
  } else {
    len = 3 + 2 * (N - 1);
  }

  f = rut_filter_new (len);
  f->ofs = (len - 1) / 2;

#ifdef PRECISE_GAUSSIAN
  for (i = 0; i < len - f->ofs; i++) {
    float gaussian = (expf (- (i*i) / (2 * variance))
                      / sqrtf (2 * M_PI * variance));
    f->data[f->ofs + i] = gaussian;
    f->data[f->ofs - i] = gaussian;
  }
#else
  /* Initialise filter to a central pulse */
  memset (f->data, 0, len * sizeof (float));
  f->data[f->ofs] = 1;

  /* Convolve filter repeatedly with a [1/6 2/3 1/6] */
  buffer = malloc (len * sizeof (float));
  for (i = 0; i < N; i++) {
    for (j = 0; j < len; j++) {
      int p = j-1, q = j+1;
      buffer[j] =
        f->data[j]*2.0/3.0
        + ((p >= 0) ? f->data[p] : 0)/6.0
        + ((q < len) ? f->data[q] : 0)/6.0;
    }
    tmp = f->data;
    f->data = buffer;
    buffer = tmp;
  }
  free (buffer);
#endif
  return f;
}

RutFilter *rut_filter_new_deriv ()
{
  RutFilter *result = rut_filter_new (3);
  result->ofs = 1;
  result->data[0] = -0.5f;
  result->data[1] = 0;
  result->data[2] = 0.5f;
  return result;
}

struct MPFilterInfo {
  RutFilter *filt;
  RutSurface *src;
  RutSurface *dest;
  int direction;
};

static void
rut_filter_conv (RutFilter *f, SurfaceView *src, SurfaceView *dest,
             float *buffer)
{
  int i, j;
  float *srcptr, *destptr, *filtptr;

  /* Do convolution into buffer */
  for (i = 0; i < dest->len; i++) {
    float v = 0;

    for (j = - f->ofs; j < (f->len - f->ofs); j++) {
      int src_idx = i - j;
      if (src_idx < 0 || src_idx >= src->len) continue;
      srcptr = src->target->data + src->ofs + (src->stride * src_idx);
      filtptr = f->data + f->ofs + j;

      v += (*filtptr) * (*srcptr);
    }
    buffer[i] = v;
  }

  /* Copy buffer into destination */
  srcptr = buffer;
  destptr = dest->target->data + dest->ofs;
  for (i = 0; i < dest->len; i++) {
    *destptr = *srcptr;
    destptr += dest->stride;
    srcptr++;
  }
}

static void
MP_filter_func (int thread_num, int threadcount, void *user_data)
{
  struct MPFilterInfo *info = (struct MPFilterInfo *) user_data;
  int first, count, i;
  float *buffer;
  SurfaceView *srcview, *destview;
  void (*set_view_func)(SurfaceView *, int);

  assert (info);
  assert (info->src);
  assert (info->dest);

  /* Allocate buffers etc. */
  srcview = surface_view_new (info->src);
  destview = surface_view_new (info->dest);

  switch (info->direction) {
  case RUT_FILTER_ROWS:
    first = thread_num * (info->src->rows / threadcount);
    count = (thread_num + 1) * (info->src->rows / threadcount) - first;
    buffer = malloc (info->src->cols * sizeof (float));
    set_view_func = surface_view_set_row;
    break;
  case RUT_FILTER_COLS:
    first = thread_num * (info->src->cols / threadcount);
    count = (thread_num + 1) * (info->src->cols / threadcount) - first;
    buffer = malloc (info->src->rows * sizeof (float));
    set_view_func = surface_view_set_col;
    break;
  default:
    abort();
  }

  /* Do convolution along each row and/or col */
  for (i = 0; i < count; i++) {
    set_view_func (srcview, first + i);
    set_view_func (destview, first + i);

    rut_filter_conv (info->filt, srcview, destview, buffer);
  }

  surface_view_destroy (srcview);
  surface_view_destroy (destview);
  free (buffer);
}

void
rut_filter_apply_mp (RutFilter *f, RutSurface *src, RutSurface *dest, int flags)
{
  struct MPFilterInfo *info;
  int in_place = ((dest == NULL) || (dest == src));

  assert (f);
  assert (src);
  if (!in_place) {
    assert (src->rows == dest->rows);
    assert (src->cols == dest->cols);
  } else {
    dest = src;
  }

  /* If no filtering to do, just memcpy from src to dest */
  if (!(flags & RUT_FILTER_ROWS || flags & RUT_FILTER_COLS)) {
    if (!in_place) {
      memcpy (dest->data, src->data,
              sizeof (float) * src->rows * src->cols);
    }
    return;
  }

  /* Set up job */
  info = malloc (sizeof (struct MPFilterInfo));
  info->filt = f;
  info->src = src;
  info->dest = dest;

  if (flags & RUT_FILTER_ROWS) {
    info->direction = RUT_FILTER_ROWS;
    rut_multiproc_task (MP_filter_func, (void *) info);

    /* If there's another convolution, we need to make sure to apply
     * it to dest in-place. */
    info->src = dest;
  }

  if (flags & RUT_FILTER_COLS) {
    info->direction = RUT_FILTER_COLS;
    rut_multiproc_task (MP_filter_func, (void *) info);
  }

  free (info);
}
