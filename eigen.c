/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2010-2012 Peter Brett <p.brett@surrey.ac.uk>
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
#include <assert.h>
#include <stdio.h>

struct mat2x2
{
  float a, b, c, d;
  /*  [ a  c ]  *
   *  [ b  d ]  */
};

#define FLOAT_EPSILON 1.1920928955078125e-07

static inline struct mat2x2
mat2x2mul (struct mat2x2 x, struct mat2x2 y)
{
  struct mat2x2 r;
  r.a = x.a * y.a + x.c * y.b;
  r.b = x.b * y.a + x.d * y.b;
  r.c = x.a * y.c + x.c * y.d;
  r.d = x.b * y.c + x.d * y.d;
  return r;
}

static inline void
chop_small_elements (struct mat2x2 *mat)
{
  if (fabsf (mat->b) < FLOAT_EPSILON * (fabsf (mat->a) + fabsf (mat->d))) {
    mat->b = 0;
  }
  if (fabsf (mat->c) < FLOAT_EPSILON * (fabsf (mat->a) + fabsf (mat->d))) {
    mat->c = 0;
  }
}

static inline float
trailing_eigenvalue (struct mat2x2 A) {
  float ta = A.a;
  float tb = A.d;
  float tab = A.b;
  float dt = (ta - tb) / 2;
  float mu;
  if (dt < 0) {
    mu = tb - tab * (tab / (dt + hypot (dt, tab)));
  } else if (dt == 0) {
    mu = tb - fabsf (tab);
  } else {
    mu = tb + tab * (tab / ((-dt) + hypot (dt, tab)));
  }

  if (FLOAT_EPSILON * fabsf (mu) > (fabsf (ta) + fabsf (tab))) {
    mu = 0;
  }

  return mu;
}

void
eigen_symm2x2 (float A11, float A12, float A22, float *l1, float *l2,
               float *e11, float *e12, float *e21, float *e22)
{
  struct mat2x2 A;
  struct mat2x2 E = {1, 0, 0, 1};
  struct mat2x2 Q, Qt;
  float c, s;

  A.a = A11; A.b = A.c = A12; A.d = A22;

  chop_small_elements (&A);

  if (!((A.b == 0.0f) || isnan (A.b))) {
    float x = A.a - trailing_eigenvalue (A);
    float z = A.b;

    /* Calculate Givens rotation coefficients. */
    if (z == 0) {
      s = 0; c = 1;
    } else if (fabsf (z) > fabsf (x)) {
      float t = -x / z;
      float u = copysign (sqrtf (1 + t*t), 1);
      s = 1.0 / u;
      c = s * t;
    } else {
      float t = -z / x;
      float u = copysign (sqrtf (1 + t*t), 1);
      c = 1.0 / u;
      s = c * t;
    }

    /* Create rotation matrices */
    Qt.a = Q.d = c;
    Qt.b = -s;
    Qt.c = s;

    Q.a = Qt.d = c;
    Q.b = s;
    Q.c = -s;

    /* Rotate source matrix and eigenvector matrix together */
    A = mat2x2mul (mat2x2mul (Q, A), Qt);
    E = Qt;
  }

  /* Return values */
  *l1 = A.a;
  *l2 = A.d;
  *e11 = E.a;
  *e12 = E.b;
  *e21 = E.c;
  *e22 = E.d;
}
