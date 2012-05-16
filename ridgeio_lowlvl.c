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
#include <string.h>
#include <assert.h>
#include <arpa/inet.h>

#include "ridgeio.h"

float
rio_htonf (float val)
{
  uint32_t i;
  memcpy (&i, &val, sizeof (i));
  i = htonl (i);
  memcpy (&val, &i, sizeof (i));
  return val;
}

float
rio_ntohf (float val)
{
  uint32_t i;
  memcpy (&i, &val, sizeof (i));
  i = ntohl (i);
  memcpy (&val, &i, sizeof (i));
  return val;
}

int
rio_write_uint32 (uint32_t val, FILE *fp)
{
  uint32_t v = htonl (val);
  return (fwrite (&v, 4, 1, fp) == 1);
}

int
rio_write_float (float val, FILE *fp) {
  float v = rio_htonf (val);
  return (fwrite (&v, 4, 1, fp) == 1);
}

int
rio_read_uint32 (uint32_t *val, FILE *fp)
{
  uint32_t v;
  if (fread (&v, 4, 1, fp) != 1) return 0;
  *val = ntohl (v);
  return 1;
}

int
rio_read_float (float *val, FILE *fp)
{
  float v;
  if (fread (&v, 4, 1, fp) != 1) return 0;
  *val = rio_ntohf (v);
  return 1;
}
