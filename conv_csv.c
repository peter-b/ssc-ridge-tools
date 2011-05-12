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

#include <config.h>

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <errno.h>

#include "ridgeconv.h"

#define FIELD_SEP ","

static int
csv_single_point (RioPoint *p, FILE *fp, bool trailing)
{
  double row, col, brightness, strength;
  int status;
  rio_point_get_subpixel (p, &row, &col);
  brightness = rio_point_get_brightness (p);
  strength = rio_point_get_strength (p);

  /* Print the point info */
  status = fprintf (fp, "%.3f%s%.3f%s%e%s%e%s",
                    row, FIELD_SEP, col, FIELD_SEP,
                    brightness, FIELD_SEP, strength,
                    trailing ? FIELD_SEP : "\n");

  return (status >= 0);
}

static int
csv_points (RioData *data, FILE *fp)
{
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    if (!csv_single_point (rio_data_get_point (data, i), fp, false))
      return 0;
  }
  return 1;
}

static int
csv_segments (RioData *data, FILE *fp)
{
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    RioSegment *s = rio_data_get_segment (data, i);
    if (!csv_single_point (rio_segment_get_start (s), fp, true))
      return 0;
    if (!csv_single_point (rio_segment_get_end (s), fp, false))
      return 0;
  }
  return 1;
}

static int
csv_lines (RioData *data, FILE *fp)
{
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    RioLine *l = rio_data_get_line (data, i);
    for (int j = 0; j < rio_line_get_length (l); j++) {
      if (!csv_single_point (rio_line_get_point (l, j), fp, false))
        return 0;
    }
    if (fputc ('\n', fp) == EOF) return 0;
  }
  return 1;
}

int
conv_csv (RioData *data, const char *filename)
{
  FILE *fp;
  if (strcmp (filename, "-") == 0) {
    /* Generate output on stdout */
    fp = stdout;
  } else {
    fp = fopen (filename, "wb");
    if (fp == NULL) return 0;
  }

  int status = 1;

  /* Set the C locale, and save the current locale to restore it
   * later.  This is so that we get predictable & consistent
   * formatting of the data in the CSV file, independent of the user's
   * preferred locale. */
  char *sv_locale = setlocale (LC_NUMERIC, "C");

  switch (rio_data_get_type (data)) {
  case RIO_DATA_POINTS:
    status = csv_points (data, fp);
    break;
  case RIO_DATA_SEGMENTS:
    status = csv_segments (data, fp);
    break;
  case RIO_DATA_LINES:
    status = csv_lines (data, fp);
    break;
  default:
    abort ();
  }

  if (!status) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Failed to generate %s: %s\n",
             filename, msg);
  }

  if (fp != stdout) {
    fclose (fp);
  }

  /* Restore the saved locale. */
  setlocale (LC_NUMERIC, sv_locale);

  return status;
}
