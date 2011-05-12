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
#include <assert.h>

#include "ridgeio.h"

#define INITIAL_LENGTH 4

int
rio_line_write (RioLine *line, FILE *fp)
{
  assert (line);

  /* Write the number of points in the line */
  if (!rio_write_uint32 (rio_line_get_length (line), fp)) return 0;

  /* Write each of the constituent points */
  for (int i = 0; i < rio_line_get_length (line); i++) {
    if (!rio_point_write (rio_line_get_point (line, i), fp)) return 0;
  }
  return 1;
}

int
rio_line_read (RioLine *line, FILE *fp)
{
  assert (line);
  uint32_t len;

  /* First empty the line */
  rio_line_clear (line);

  /* Read the number of points in the line */
  if (!rio_read_uint32 (&len, fp)) return 0;

  /* Allocate the required memory */
  rio_line_init (line, len);

  /* Read in each of the constituent points */
  for (int i = 0; i < len; i++) {
    if (!rio_point_read (rio_line_new_point (line), fp)) return 0;
  }
  return 1;
}
