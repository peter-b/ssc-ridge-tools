#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "ridgetool.h"

struct MPMetricsSSInfo {
  Surface *Dx, *Dy, *Dxx, *Dyy, *Dxy, *Lp, *Lpp, *RnormL;
  float (*norm_func)(float, float, float);
  float scale;
};

static float
metric_Mnorm (float Lpp, float Lqq, float scale)
{
  /* normalised maximum absolute principal curvature */
  return scale * fmaxf (fabsf (Lpp), fabsf (Lqq));
}

static float
metric_Nnorm (float Lpp, float Lqq, float scale)
{
  /* square of normalised square principal curvature difference */
  float d = Lpp * Lpp - Lqq * Lqq;
  float t2 = scale*scale;
  return t2*t2 * d*d;
}

static float
metric_Anorm (float Lpp, float Lqq, float scale)
{
  /* square of normalised principal curvature difference */
  float d = Lpp - Lqq;
  return scale*scale * d*d;
}

static void
MP_metrics_SS_func (int thread_num, int threadcount,
                    void *user_data)
{
  struct MPMetricsSSInfo *info = (struct MPMetricsSSInfo *) user_data;
  int first_row, num_rows, row, col;
  float l1, l2, e11, e12, e21, e22, e1, e2;

  first_row = thread_num * (info->RnormL->rows / threadcount);
  num_rows = (thread_num + 1) * (info->RnormL->rows / threadcount) - first_row;

  for (row = first_row; row < first_row + num_rows; row++) {
    for (col = 0; col < info->RnormL->rows; col++) {
      /* Find principal curvatures */
      eigen_symm2x2 (SURFACE_REF (info->Dyy, row, col),
                     SURFACE_REF (info->Dxy, row, col),
                     SURFACE_REF (info->Dxx, row, col),
                     &l1, &l2, &e11, &e12, &e21, &e22);

      /* Choose best direction and calculate Lp and Lpp */
      if (fabsf (l1) > fabsf (l2)) {
        SURFACE_REF (info->Lpp, row, col) = l1;
        e1 = e11; e2 = e12;
      } else {
        SURFACE_REF (info->Lpp, row, col) = l2;
        e1 = e21; e2 = e22;
      }
      SURFACE_REF (info->Lp, row, col) =
        (e1 * SURFACE_REF (info->Dy, row, col)
         + e2 * SURFACE_REF (info->Dx, row, col));

      /* Calculate ridge strength metric */
      SURFACE_REF (info->RnormL, row, col) =
        info->norm_func (l1, l2, info->scale);
    }
  }
}

void
MP_metrics_SS (Surface *src, float scale, int norm,
               Surface **Lp, Surface **Lpp, Surface **RnormL)
{
  Filter *deriv;
  struct MPMetricsSSInfo *info;

  assert (src);
  assert (Lp);
  assert (Lpp);
  assert (RnormL);

  /* Setup job info structure */
  info = malloc (sizeof (struct MPMetricsSSInfo));

  info->Dx     = surface_new_like (src);
  info->Dy     = surface_new_like (src);
  info->Dxx    = surface_new_like (src);
  info->Dyy    = surface_new_like (src);
  info->Dxy    = surface_new_like (src);
  info->Lp     = surface_new_like (src);
  info->Lpp    = surface_new_like (src);
  info->RnormL = surface_new_like (src);
  info->scale  = scale;
  switch (norm) {
  case METRICS_ANORM:
    info->norm_func = metric_Anorm;
    break;
  case METRICS_MNORM:
    info->norm_func = metric_Mnorm;
    break;
  case METRICS_NNORM:
    info->norm_func = metric_Nnorm;
    break;
  default:
    abort ();
  }

  /* Calculate derivatives */
  deriv = filter_new_deriv ();

  MP_filter (deriv, src,      info->Dx, FILTER_FLAG_ROWS);
  MP_filter (deriv, src,      info->Dy, FILTER_FLAG_COLS);
  MP_filter (deriv, info->Dx, info->Dxx, FILTER_FLAG_ROWS);
  MP_filter (deriv, info->Dy, info->Dyy, FILTER_FLAG_COLS);
  MP_filter (deriv, info->Dx, info->Dxy, FILTER_FLAG_COLS);

  filter_destroy (deriv);

  /* Do metrics generation */
  MP_task (MP_metrics_SS_func, (void *) info);

  /* Set result pointers */
  *Lp = info->Lp;
  *Lpp = info->Lpp;
  *RnormL = info->RnormL;

  /* Clean up */
  surface_destroy (info->Dx);
  surface_destroy (info->Dy);
  surface_destroy (info->Dxx);
  surface_destroy (info->Dyy);
  surface_destroy (info->Dxy);
  free (info);
}
