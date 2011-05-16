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
#include <errno.h>
#include <png.h>
#include <tiffio.h>

#include "ridgeconv.h"

#define MIN(x,y) ((x < y) ? x : y)

/* Describes a ridge mask. Each pixel in the mask is set to 0xff if it
 * contains a ridge, and 0x00 otherwise. */
struct Mask {
  int height, width;
  uint8_t *data;
};

/* Marks the mask pixel at (row, col) as containing a ridge. */
static inline void
mask_set (struct Mask *m, int row, int col)
{
  m->data[row*m->width + col] = 0xff;
}

/* Adds the point p to the mask m */
static void
mask_point (RioPoint *p, struct Mask *m)
{
  int row, col;
  rio_point_get_position (p, &row, &col);
  mask_set (m, row >> 7, col >> 7);
}

/* Adds the segment joining p and q to the mask m */
static void
mask_segment (RioPoint *p, RioPoint *q, struct Mask *m)
{
  int row[2], col[2];
  rio_point_get_position (p, &row[0], &col[0]);
  rio_point_get_position (p, &row[1], &col[1]);
  row[0] = MIN(row[0], row[1]);
  col[0] = MIN(col[0], col[1]);
  mask_set (m, row[0] >> 7, col[0] >> 7);
}

/* Create a mask from ridge data */
static struct Mask *
mask_create (RioData *data)
{
  /* Determine original image height & width */
  uint32_t height, width;
  int status = rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_ROWS, &height)
    && rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_COLS, &width);
  if (!status) {
    fprintf (stderr, "ERROR: Could not determine image dimensions.\n");
    return NULL;
  }

  /* Create mask data structure */
  struct Mask *m = malloc (sizeof (struct Mask));
  m->height = (int) height;
  m->width = (int) width;
  m->data = malloc (width * height);
  memset (m->data, 0, width * height);

  /* Populate mask */
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    switch (rio_data_get_type (data)) {
    case RIO_DATA_POINTS:
      mask_point (rio_data_get_point (data, i), m);
      break;
    case RIO_DATA_SEGMENTS:
      {
        RioSegment *s = rio_data_get_segment (data, i);
        mask_segment (rio_segment_get_start (s), rio_segment_get_end (s), m);
      }
      break;
    case RIO_DATA_LINES:
      {
        RioLine *l = rio_data_get_line (data, i);
        RioPoint *prev = NULL, *curr;
        for (int j = 0; j < rio_line_get_length (l); j++) {
          curr = rio_line_get_point (l, j);
          if (prev != NULL) {
            mask_segment (prev, curr, m);
          }
          prev = curr;
        }
      }
      break;
    default:
      abort();
    }
  }

  return m;
}

/* Free a mask structure */
static void
mask_free (struct Mask *m) {
  if (!m) return;
  free (m->data);
  free (m);
}

/* Create a PNG format mask with the given filename from ridge data */
int
conv_png (RioData *data, const char *filename)
{
  FILE *fp = NULL;
  struct Mask *mask = NULL;
  uint8_t **row_ptrs;
  png_structp png_ctx = NULL;
  png_infop png_write_ctx = NULL;
  int status = 0;

  /* Create mask */
  mask = mask_create (data);
  if (!mask) goto png_return;

  /* Open output file */
  fp = fopen (filename, "wb");
  if (fp == NULL) {
    fprintf (stderr, "ERROR: Could not open %s for writing: %s\n",
             filename, strerror (errno));
    goto png_return;
  }

  /* Set up libpng. */
  png_ctx = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  png_write_ctx = png_create_info_struct (png_ctx);

  if (setjmp (png_jmpbuf (png_ctx))) {
    goto png_return;
  }

  png_init_io (png_ctx, fp);

  /* Set up file headers */
  png_set_IHDR (png_ctx, png_write_ctx, mask->width, mask->height,
                8, PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

  /* Array of row pointers */
  row_ptrs = malloc (mask->height * sizeof (uint8_t *));
  for (int i = 0; i < mask->height; i++) {
    row_ptrs[i] = mask->data + i * mask->width;
  }
  png_set_rows (png_ctx, png_write_ctx, row_ptrs);

  /* Write output */
  png_write_png (png_ctx, png_write_ctx, PNG_TRANSFORM_IDENTITY, NULL);
  status = 1;

 png_return:
  if (png_write_ctx) png_destroy_write_struct (&png_ctx, &png_write_ctx);
  if (fp) fclose (fp);
  if (row_ptrs) free (row_ptrs);
  mask_free (mask);
  return status;
}

/* Create a TIFF image mask with the given filename from ridge data */
int
conv_tif (RioData *data, const char *filename)
{
  struct Mask *mask = NULL;
  TIFF *tif = NULL;
  int status = 0;
  int rows_per_strip, num_strips;

  /* Create mask */
  mask = mask_create (data);
  if (!mask) goto tif_return;

  /* Open TIFF file */
  tif = TIFFOpen (filename, "wb");
  if (!tif) {
    fprintf (stderr, "ERROR: Could not open %s for writing\n", filename);
    goto tif_return;
  }

  /* Set TIFF tags */
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, mask->width);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, mask->height);
  TIFFSetField (tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField (tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

  rows_per_strip = TIFFDefaultStripSize (tif, 0);
  TIFFSetField (tif, TIFFTAG_ROWSPERSTRIP, rows_per_strip);
  num_strips = TIFFNumberOfStrips (tif);

  /* Copy data into TIFF strips */
  for (int row = 0, strip = 0; strip < num_strips; strip++) {
    int num_rows = MIN (rows_per_strip, mask->height - row);
    int size = num_rows * mask->width;
    char *buffer = (char *) mask->data + row * mask->width;
    if (TIFFWriteEncodedStrip (tif, strip, buffer, size) == -1) {
      fprintf (stderr, "ERROR: Could not write TIFF strip %i/%i to %s\n",
               strip+1, num_strips, filename);
      goto tif_return;
    }
    row += rows_per_strip;
  }

  status = 1;
 tif_return:
  if (tif) TIFFClose (tif);
  mask_free (mask);
  return status;
}
