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
#include <stdio.h>
#include <assert.h>
#include <errno.h>

#include "ridgetool.h"
#include "ridgeio.h"

static float
surface_interpolate (RutSurface *s, int r1, int c1, int r2, int c2,
                     unsigned char d)
{
  return LINTERP ((float) d / 128.0f,
                  RUT_SURFACE_REF (s, r1, c1),
                  RUT_SURFACE_REF (s, r2, c2));
}

static int
export_point (RidgePointsSS *ridges, RutSurface *image, RutSurface *RnormL,
              int row, int col, int dir, FILE *fp)
{
  int row2, col2;
  unsigned char d;
  float brightness, strength;
  RioPoint p;

  /* Canonicalise position */
  switch (dir) {
  case EDGE_FLAG_SOUTH:
    row++;
    dir = EDGE_FLAG_NORTH;
    break;
  case EDGE_FLAG_EAST:
    col++;
    dir = EDGE_FLAG_WEST;
    break;
  default:
    break;
  }

  /* We now know the coordinates of the start of the interpolation
   * line. Calculate the coordinates of the other end of the line, and
   * get the distance. Populate the RioPoint structure with the
   * subpixel position. */
  row2 = row;
  col2 = col;
  switch (dir) {
  case EDGE_FLAG_NORTH:
    col2++;
    d = RIDGE_POINTS_SS_PTR(ridges, row, col)->north;
    rio_point_set_position (&p, (row << 7), (col << 7) + d);
    break;
  case EDGE_FLAG_WEST:
    row2++;
    d = RIDGE_POINTS_SS_PTR(ridges, row, col)->west;
    rio_point_set_position (&p, (row << 7) + d, (col << 7));
    break;
  default:
    abort ();
  }

  /* Interpolate brightness & strength */
  brightness = surface_interpolate (image, row, col, row2, col2, d);
  strength = surface_interpolate (RnormL, row, col, row2, col2, d);
  rio_point_set_brightness (&p, brightness);
  rio_point_set_strength (&p, strength);

  /* Output */
  return rio_point_write (&p, fp);
}

int
export_image_size (RutSurface *image, FILE *fp)
{
  RioData *d = rio_data_new (RIO_DATA_POINTS);
  rio_data_set_metadata_uint32 (d, RIO_KEY_IMAGE_ROWS, (uint32_t) image->rows);
  rio_data_set_metadata_uint32 (d, RIO_KEY_IMAGE_COLS, (uint32_t) image->cols);
  int s = (fseek (fp, 0, SEEK_END) != -1) && rio_data_write_metadata (d, fp);
  rio_data_destroy (d);
  return s;
}

int
export_points (RidgePointsSS *ridges, RutSurface *image,
               RutSurface *RnormL, const char *filename)
{
  int errsv;
  assert (ridges);
  assert (image);
  assert (RnormL);
  assert (filename);

  FILE *fp = fopen (filename, "wb");
  if (!fp) goto export_fail;

  int len = 0;

  /* First, write the header with the length field zeroed out. */
  rio_data_write_header (RIO_DATA_POINTS, len, fp);

  for (int row = 0; row < ridges->rows; row++) {
    for (int col = 0; col < ridges->cols; col++) {
      RidgePointsSSEntry *entry = RIDGE_POINTS_SS_PTR(ridges, row, col);
      if (entry->flags & EDGE_FLAG_NORTH) {
        if (!export_point (ridges, image, RnormL,
                           row, col, EDGE_FLAG_NORTH, fp)) goto export_fail;
        len++;
      }
      if (entry->flags & EDGE_FLAG_WEST) {
        if (!export_point (ridges, image, RnormL,
                           row, col, EDGE_FLAG_WEST, fp)) goto export_fail;
        len++;
      }
    }
  }

  /* Rewind the file and rewrite the header with the actual length. */
  if (fseek (fp, 0, SEEK_SET) == -1) goto export_fail;
  rio_data_write_header (RIO_DATA_POINTS, len, fp);

  /* Add metadata block to end of file */
  export_image_size (image, fp);

  fclose (fp);

  return 1;

 export_fail:
  errsv = errno;
  if (fp) fclose (fp);
  errno = errsv;
  return 0;
}

int
export_segments (RidgePointsSS *ridges, RutSurface *image,
                 RutSurface *RnormL, const char *filename)
{
  int errsv;
  assert (ridges);
  assert (image);
  assert (RnormL);
  assert (filename);

  FILE *fp = fopen (filename, "wb");
  if (!fp) goto export_fail;

  int len = 0;

  /* First, write the header with the length field zeroed out. */
  rio_data_write_header (RIO_DATA_SEGMENTS, len, fp);

  for (int row = 0; row < ridges->rows; row++) {
    for (int col = 0; col < ridges->cols; col++) {
      RidgePointsSSEntry *entry = RIDGE_POINTS_SS_PTR(ridges, row, col);
      if (count_bits_set (entry->flags) != 2) continue;

      /* FIXME: assumes lots about the flag structure */
      for (int i = 0; i < 4; i++) {
        int edge = 1 << i;
        if (!(entry->flags & edge)) continue;
        if (!export_point (ridges, image, RnormL,
                           row, col, entry->flags & edge, fp)) goto export_fail;
      }

      len++;
    }
  }

  /* Rewind the file and rewrite the header with the actual length. */
  if (fseek (fp, 0, SEEK_SET) == -1) goto export_fail;
  rio_data_write_header (RIO_DATA_SEGMENTS, len, fp);

  /* Add metadata block to end of file */
  export_image_size (image, fp);

  fclose (fp);

  return 1;

 export_fail:
  errsv = errno;
  if (fp) fclose (fp);
  errno = errsv;
  return 0;
}

int
export_lines (RidgeLinesSS *lines, RidgePointsSS *ridges,
              RutSurface *image, RutSurface *RnormL, const char *filename)
{
  int errsv;
  assert (ridges);
  assert (image);
  assert (RnormL);
  assert (filename);

  FILE *fp = fopen (filename, "wb");
  if (!fp) goto export_fail;

  int num_lines = 0;

  /* First, write the header with the length field zeroed out. */
  rio_data_write_header (RIO_DATA_LINES, num_lines, fp);

  for (int i = 0; i < ridges->rows; i++) {
    for (int j = 0; j < ridges->cols; j++) {
      RidgeLinesSSEntry *lentry = RIDGE_LINES_SS_PTR(lines, i, j);
      RidgePointsSSEntry *pentry = RIDGE_POINTS_SS_PTR(ridges, i, j);
      int len_pos, num_points = 0;

      /* Exclude points that aren't at start of line or aren't
       * ridge segments. */
      if (count_bits_set (pentry->flags) != 2) continue;
      if (ridge_lines_SS_entry_prev (lentry)) continue;

      /* Output an empty line length field. We'll come back & fill it
       * in later. */
      len_pos = ftell (fp);
      if (len_pos == -1) goto export_fail;
      if (!rio_write_uint32 (0, fp)) goto export_fail;

      /* Walk along the line */
      RidgeLinesSSEntry *lprev = NULL, *lnext = NULL;
      int row = i, col = j;
      int prev_row = -1, prev_col = -1;
      while (lentry != NULL) {
        int edge_to_prev = 0;

        /* Work out direction towards previous point, if this isn't
         * the first point. */
        if (lprev) {
          if (prev_row == row) {
            if (prev_col - col > 0) {
              edge_to_prev = EDGE_FLAG_EAST;
            } else {
              edge_to_prev = EDGE_FLAG_WEST;
            }
          } else {
            if (prev_row - row > 0) {
              edge_to_prev = EDGE_FLAG_SOUTH;
            } else {
              edge_to_prev = EDGE_FLAG_NORTH;
            }
          }
        }

        /* FIXME: assumes lots about the flag structure */
        for (int i = 0; i < 4; i++) {
          int edge = (1 << i);
          /* Skip the edge pointing towards the previous edge */
          if (edge_to_prev == edge) continue;
          if (!(pentry->flags & edge)) continue;
          if (!export_point (ridges, image, RnormL,
                             row, col, pentry->flags & edge, fp))
            goto export_fail;
          num_points++;
        }

        /* Find next point in line & interate */
        lprev = lentry;
        prev_row = row; prev_col = col;

        lnext = ridge_lines_SS_entry_next (lentry);

        assert (lnext != lentry);

        RidgePointsSSEntry *pnext = NULL;
        if (lnext) {
          ridge_lines_SS_entry_get_position (lines, lnext, &row, &col);
          pnext = RIDGE_POINTS_SS_PTR (ridges, row, col);
          assert (count_bits_set (pnext->flags) == 2);
        }

        lentry = lnext;
        pentry = pnext;
      }

      /* Rewind the file and write in the number of points in the line. */
      if (fseek (fp, len_pos, SEEK_SET) == -1
          || !rio_write_uint32 (num_points, fp)
          || fseek (fp, 0, SEEK_END) == -1) goto export_fail;

      num_lines++;
    }
  }

  /* Rewind the file and rewrite the header with the actual length. */
  if (fseek (fp, 0, SEEK_SET) == -1) goto export_fail;
  rio_data_write_header (RIO_DATA_LINES, num_lines, fp);

  /* Add metadata block to end of file */
  export_image_size (image, fp);

  fclose (fp);

  return 1;

 export_fail:
  errsv = errno;
  if (fp) fclose (fp);
  errno = errsv;
  return 0;
}
