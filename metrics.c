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
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "ridgetool.h"

struct MPMetricsSSInfo {
  RutSurface *Dx, *Dy, *Dxx, *Dyy, *Dxy, *Lp, *Lpp, *RnormL;
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
    for (col = 0; col < info->RnormL->cols; col++) {
      float lpp;
      /* Find principal curvatures */
      eigen_symm2x2 (RUT_SURFACE_REF (info->Dyy, row, col),
                     RUT_SURFACE_REF (info->Dxy, row, col),
                     RUT_SURFACE_REF (info->Dxx, row, col),
                     &l1, &l2, &e11, &e12, &e21, &e22);

      /* Choose best direction and calculate Lp and Lpp */
      if (fabsf (l1) > fabsf (l2)) {
        lpp = l1;
        e1 = e11; e2 = e12;
      } else {
        lpp = l2;
        e1 = e21; e2 = e22;
      }

      if (info->Lpp) RUT_SURFACE_REF (info->Lpp, row, col) = lpp;
      if (info->Lp) {
        RUT_SURFACE_REF (info->Lp, row, col) =
          (e1 * RUT_SURFACE_REF (info->Dy, row, col)
           + e2 * RUT_SURFACE_REF (info->Dx, row, col));
      }

      /* Calculate ridge strength metric */
      if (info->RnormL) {
        RUT_SURFACE_REF (info->RnormL, row, col) =
          info->norm_func (l1, l2, info->scale);
      }
    }
  }
}

void
MP_metrics_SS (RutSurface *src, float scale, int norm,
               RutSurface *Lp, RutSurface *Lpp, RutSurface *RnormL)
{
  RutFilter *deriv;
  struct MPMetricsSSInfo *info;

  assert (src);

  /* Setup job info structure */
  info = malloc (sizeof (struct MPMetricsSSInfo));

  info->Lp     = Lp;
  info->Lpp    = Lpp;
  info->RnormL = RnormL;
  info->Dx     = rut_surface_new_like (src);
  info->Dy     = rut_surface_new_like (src);
  info->Dxx    = rut_surface_new_like (src);
  info->Dyy    = rut_surface_new_like (src);
  info->Dxy    = rut_surface_new_like (src);
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
  deriv = rut_filter_new_deriv ();

  rut_filter_surface_mp (deriv, src,      info->Dx, RUT_SURFACE_ROWS);
  rut_filter_surface_mp (deriv, src,      info->Dy, RUT_SURFACE_COLS);
  rut_filter_surface_mp (deriv, info->Dx, info->Dxx, RUT_SURFACE_ROWS);
  rut_filter_surface_mp (deriv, info->Dy, info->Dyy, RUT_SURFACE_COLS);
  rut_filter_surface_mp (deriv, info->Dx, info->Dxy, RUT_SURFACE_COLS);

  rut_filter_destroy (deriv);

  /* Do metrics generation */
  rut_multiproc_task (MP_metrics_SS_func, (void *) info);

  /* Clean up */
  rut_surface_destroy (info->Dx);
  rut_surface_destroy (info->Dy);
  rut_surface_destroy (info->Dxx);
  rut_surface_destroy (info->Dyy);
  rut_surface_destroy (info->Dxy);
  free (info);
}

void
MP_metrics (RutScaleSpace *src, int norm,
            RutScaleSpace *Lp, RutScaleSpace *Lpp, RutScaleSpace *RnormL,
            RutScaleSpace *dRnormL, RutScaleSpace *ddRnormL)
{
  assert (src);

  /* Generate metrics in the image plane */
  for (int i = 0; i < src->n_scales; i++) {
    RutSurface *ssrc, *sLp = NULL, *sLpp = NULL, *sRnormL = NULL;
    ssrc = rut_scale_space_get_surface (src, RUT_SCALE_SPACE_SCALE, i);
    if (Lp)
      sLp = rut_scale_space_get_surface (Lp, RUT_SCALE_SPACE_SCALE, i);
    if (Lpp)
      sLpp = rut_scale_space_get_surface (Lpp, RUT_SCALE_SPACE_SCALE, i);
    if (RnormL)
      sRnormL = rut_scale_space_get_surface (RnormL, RUT_SCALE_SPACE_SCALE, i);

    MP_metrics_SS (ssrc, src->scales[i], norm, sLp, sLpp, sRnormL);

    rut_surface_destroy (sLp);
    rut_surface_destroy (sLpp);
    rut_surface_destroy (sRnormL);
  }

  /* Generate inter-scale metrics. */
  if (!RnormL) return;

  RutFilter *deriv = rut_filter_new_deriv ();

  /* First derivative w.r.t. scale of ridge strength */
  if (dRnormL) {
    rut_filter_scale_space_mp (deriv, RnormL, dRnormL, RUT_SCALE_SPACE_SCALE);
  }

  /* Second derivative w.r.t. scale of ridge strength */
  if (ddRnormL) {
    if (dRnormL) { /* If we already calculated first deriv, use it */
      rut_filter_scale_space_mp (deriv, dRnormL, ddRnormL,
                                 RUT_SCALE_SPACE_SCALE);
    } else {
      rut_filter_scale_space_mp (deriv, RnormL, ddRnormL,
                                 RUT_SCALE_SPACE_SCALE);
      rut_filter_scale_space_mp (deriv, ddRnormL, NULL,
                                 RUT_SCALE_SPACE_SCALE);
    }
  }

  rut_filter_destroy (deriv);
}
