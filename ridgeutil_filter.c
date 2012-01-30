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

#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ridgeutil.h"

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

  assert (variance >= 0); /* Sanity check */

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
rut_filter_conv (RutFilter *f, RutSurface *src, RutSurface *dest,
                 float *buffer)
{
  int i, j;
  float *srcptr, *filtptr;

  /* Do convolution into buffer */
  for (i = 0; i < dest->cols; i++) {
    float v = 0;

    for (j = - f->ofs; j < (f->len - f->ofs); j++) {
      int src_idx = i - j;
      if (UNLIKELY (src_idx < 0)) src_idx = 0;
      if (UNLIKELY (src_idx >= src->cols)) src_idx = src->cols - 1;
      filtptr = f->data + f->ofs + j;

      v += (*filtptr) * RUT_SURFACE_REF (src, 0, src_idx);
    }
    buffer[i] = v;
  }

  /* Copy buffer into destination */
  srcptr = buffer;
  for (i = 0; i < dest->cols; i++) {
    RUT_SURFACE_REF (dest, 0, i) = *srcptr;
    srcptr++;
  }
}

static void
MP_filter_func (int thread_num, int threadcount, void *user_data)
{
  struct MPFilterInfo *info = (struct MPFilterInfo *) user_data;
  int first, count, i;
  float *buffer;
  RutSurface *src, *dest;
  RutSurface *srcview, *destview;
  RutExtents extents;

  assert (info);
  assert (info->src);
  assert (info->dest);

  src = rut_surface_new_view (info->src);
  dest = rut_surface_new_view (info->dest);

  /* Transpose the src/dest surfaces if necessary to ensure that we
   * always convolve along rows. */
  if (info->direction == RUT_SURFACE_COLS) {
    rut_surface_transpose (src);
    rut_surface_transpose (dest);
  }

  /* Allocate buffers etc. */
  first = thread_num * (src->rows / threadcount);
  count = (thread_num + 1) * (src->rows / threadcount) - first;
  buffer = malloc (src->cols * sizeof (float));

  /* Create views of single rows */
  extents.top = first;
  extents.left = 0;
  extents.height = 1;
  extents.width = src->cols;
  srcview = rut_surface_new_view_extents (src, &extents);
  destview = rut_surface_new_view_extents (dest, &extents);

  /* Do convolution along each row and/or col */
  for (i = 0; i < count; i++) {
    rut_filter_conv (info->filt, srcview, destview, buffer);

    /* FIXME naughty pointer hacks to save recreating surface views */
    srcview->data += src->rowstep;
    destview->data += dest->rowstep;
  }

  rut_surface_destroy (srcview);
  rut_surface_destroy (destview);
  rut_surface_destroy (src);
  rut_surface_destroy (dest);
  free (buffer);
}

void
rut_filter_surface_mp (RutFilter *f, RutSurface *src, RutSurface *dest,
                       int flags)
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

  /* If no filtering to do, just copy from src to dest */
  if (!(flags & RUT_SURFACE_ROWS || flags & RUT_SURFACE_COLS)) {
    if (!in_place) {
      for (int i = 0; i < src->rows; i++) {
        for (int j = 0; i < src->cols; i++) {
          RUT_SURFACE_REF (dest, i, j) = RUT_SURFACE_REF (src, i, j);
        }
      }
    }
    return;
  }

  /* Set up job */
  info = malloc (sizeof (struct MPFilterInfo));
  info->filt = f;
  info->src = src;
  info->dest = dest;

  if (flags & RUT_SURFACE_ROWS) {
    info->direction = RUT_SURFACE_ROWS;
    rut_multiproc_task (MP_filter_func, (void *) info);

    /* If there's another convolution, we need to make sure to apply
     * it to dest in-place. */
    info->src = dest;
  }

  if (flags & RUT_SURFACE_COLS) {
    info->direction = RUT_SURFACE_COLS;
    rut_multiproc_task (MP_filter_func, (void *) info);
  }

  free (info);
}

void
rut_filter_scale_space_mp (RutFilter *f, RutScaleSpace *src,
                           RutScaleSpace *dest, int flags)
{
  RutSurface *ssrc, *sdest;
  int sflags;
  int in_place = ((dest == NULL) || (dest == src));

  assert (f);
  assert (src);
  if (!in_place) {
    assert (src->rows == dest->rows);
    assert (src->cols == dest->cols);
    assert (src->n_scales == dest->n_scales);
  } else {
    dest = src;
  }

  /* If no filtering to do, just copy from src to dest */
  if ((flags & (RUT_SCALE_SPACE_ROWS | RUT_SCALE_SPACE_COLS |
                RUT_SCALE_SPACE_SCALE)) == 0) {
    if (!in_place) {
      for (int i = 0; i < src->n_scales; i++) {
        for (int j = 0; j < src->rows; j++) {
          for (int k = 0; k < src->cols; k++) {
            /* FIXME Ewww nested loops */
            RUT_SCALE_SPACE_REF (dest, j, k, i) =
              RUT_SCALE_SPACE_REF (src, j, k, i);
          }
        }
      }
    }
    return;
  }

  /* FIXME This is a little bit ugly, but it's good enough because
   * it's actually pretty cheap to generate views, and because the
   * most common operations are either in scale plane or along scale
   * axis only. */

  if (flags & (RUT_SCALE_SPACE_ROWS | RUT_SCALE_SPACE_COLS)) {
    /* Flags for in-surface filtering step */
    sflags = ((flags & RUT_SCALE_SPACE_COLS ? RUT_SURFACE_COLS : 0) |
              (flags & RUT_SCALE_SPACE_ROWS ? RUT_SURFACE_ROWS : 0));

    for (int i = 0; i < src->n_scales; i++) {
      /* Create surfaces for filtering */
      ssrc = rut_scale_space_get_surface (src, RUT_SCALE_SPACE_SCALE, i);
      sdest = rut_scale_space_get_surface (dest, RUT_SCALE_SPACE_SCALE, i);

      /* Filter within surface */
      rut_filter_surface_mp (f, ssrc, sdest, sflags);

      /* Destroy surfaces */
      rut_surface_destroy (ssrc);
      rut_surface_destroy (sdest);
    }

    /* If the scale-axis pass follows, it needs to be in-place. */
    src = dest;
  }

  /* Second pass is always in-place on destination space. */
  if (flags & RUT_SCALE_SPACE_SCALE) {
    for (int i = 0; i < src->rows; i++) {
      /* Create surfaces for filtering */
      ssrc = rut_scale_space_get_surface (src, RUT_SCALE_SPACE_SCALE, i);
      sdest = rut_scale_space_get_surface (dest, RUT_SCALE_SPACE_ROWS, i);

      /* Filter within surface */
      rut_filter_surface_mp (f, ssrc, sdest, RUT_SURFACE_COLS);

      /* Destroy surfaces */
      rut_surface_destroy (ssrc);
      rut_surface_destroy (sdest);
    }
  }
}
