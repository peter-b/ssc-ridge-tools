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
#include <math.h>
#include <assert.h>

#include "ridgeio.h"

#define POSITION_SCALE 128.0

void
rio_point_get_position (RioPoint *point, int *row, int *col)
{
  assert (point);
  if (row) *row = point->row;
  if (col) *col = point->col;
}

void
rio_point_get_subpixel (RioPoint *point, double *row, double *col)
{
  assert (point);
  if (row) *row = (double) point->row / POSITION_SCALE;
  if (col) *col = (double) point->col / POSITION_SCALE;
}

float
rio_point_get_brightness (RioPoint *point)
{
  assert (point);
  return point->brightness;
}

float
rio_point_get_strength (RioPoint *point)
{
  assert (point);
  return point->strength;
}

void
rio_point_set_position (RioPoint *point, int row, int col)
{
  assert (point);
  point->row = row;
  point->col = col;
}

void
rio_point_set_subpixel (RioPoint *point, double row, double col)
{
  assert (point);
  point->row = (uint32_t) llrint (row * POSITION_SCALE);
  point->col = (uint32_t) llrint (col * POSITION_SCALE);
}

void
rio_point_set_brightness (RioPoint *point, float brightness)
{
  point->brightness = brightness;
}

void
rio_point_set_strength (RioPoint *point, float strength)
{
  point->strength = strength;
}

int
rio_point_write (RioPoint *point, FILE *fp)
{
  assert (point);
  return (rio_write_uint32 (point->row, fp)
          && rio_write_uint32 (point->col, fp)
          && rio_write_float (point->brightness, fp)
          && rio_write_float (point->strength, fp));
}

int
rio_point_read (RioPoint *point, FILE *fp)
{
  assert (point);
  return (rio_read_uint32 (&point->row, fp)
          && rio_read_uint32 (&point->col, fp)
          && rio_read_float (&point->brightness, fp)
          && rio_read_float (&point->strength, fp));
}
