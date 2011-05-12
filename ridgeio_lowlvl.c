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
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "ridgeio.h"

int
rio_write_uint32 (uint32_t val, FILE *fp)
{
  for (int i = 0; i < 4; i++) {
    int s = fputc (val >> ((3-i)*8), fp);
    if (s == EOF) return 0;
  }
  return 1;
}

int
rio_write_float (float val, FILE *fp) {
  union {
    uint32_t i;
    float f;
  } u;
  u.f = val;
  return rio_write_uint32 (u.i, fp);
}

int
rio_read_uint32 (uint32_t *val, FILE *fp)
{
  uint32_t v = 0;
  for (int i = 0; i < 4; i ++) {
    int b = fgetc (fp);
    if (b == EOF) return 0;
    v = (v << 8) + b;
  }
  *val = v;
  return 1;
}

int
rio_read_float (float *val, FILE *fp)
{
  union {
    uint32_t i;
    float f;
  } u;
  if (!rio_read_uint32 (&u.i, fp)) return 0;
  *val = u.f;
  return 1;
}
