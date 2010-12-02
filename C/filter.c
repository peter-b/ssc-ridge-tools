#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ridgetool.h"

Filter *
filter_new (int len) {
  Filter *result = malloc (sizeof(Filter));
  result->len = len;
  result->ofs = 0;
  result->data = malloc (len * sizeof (float));
  return result;
}

void
filter_destroy (Filter *f) {
  if (!f) return;
  free (f->data);
  free (f);
}

Filter *
filter_new_gaussian (float variance)
{
  int N = lrintf (3 * variance);
  int len;
  int i, j;
  Filter *f;
  float *buffer, *tmp;

  if (N < 1) {
    return NULL;
  } else {
    len = 3 + 2 * (N - 1);
  }

  f = filter_new (len);
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

Filter *filter_new_deriv ()
{
  Filter *result = filter_new (3);
  result->ofs = 1;
  result->data[0] = -0.5f;
  result->data[1] = 0;
  result->data[2] = 0.5f;
  return result;
}

struct MPFilterInfo {
  Filter *filt;
  Surface *src;
  Surface *dest;
  int direction;
};

static void
filter_conv (Filter *f, SurfaceView *src, SurfaceView *dest,
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
  case FILTER_FLAG_ROWS:
    first = thread_num * (info->src->rows / threadcount);
    count = (thread_num + 1) * (info->src->rows / threadcount) - first;
    buffer = malloc (info->src->cols * sizeof (float));
    set_view_func = surface_view_set_row;
    break;
  case FILTER_FLAG_COLS:
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

    filter_conv (info->filt, srcview, destview, buffer);
  }

  surface_view_destroy (srcview);
  surface_view_destroy (destview);
  free (buffer);
}

void
MP_filter (Filter *f, Surface *src, Surface *dest, int flags)
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
  if (!(flags & FILTER_FLAG_ROWS || flags & FILTER_FLAG_COLS)) {
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

  if (flags & FILTER_FLAG_ROWS) {
    info->direction = FILTER_FLAG_ROWS;
    MP_task (MP_filter_func, (void *) info);

    /* If there's another convolution, we need to make sure to apply
     * it to dest in-place. */
    info->src = dest;
  }

  if (flags & FILTER_FLAG_COLS) {
    info->direction = FILTER_FLAG_COLS;
    MP_task (MP_filter_func, (void *) info);
  }

  free (info);
}
