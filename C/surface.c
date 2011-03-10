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
