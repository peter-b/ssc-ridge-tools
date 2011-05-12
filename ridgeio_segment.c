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

#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "ridgeio.h"

RioPoint *
rio_segment_get_start (RioSegment *segment)
{
  assert (segment);
  return &segment->start;
}

RioPoint *
rio_segment_get_end (RioSegment *segment)
{
  assert (segment);
  return &segment->end;
}

int
rio_segment_write (RioSegment *segment, FILE *fp)
{
  return (rio_point_write (rio_segment_get_start (segment), fp)
          && rio_point_write (rio_segment_get_end (segment), fp));
}

int
rio_segment_read (RioSegment *segment, FILE *fp)
{
  return (rio_point_read (rio_segment_get_start (segment), fp)
          && rio_point_read (rio_segment_get_end (segment), fp));
}
