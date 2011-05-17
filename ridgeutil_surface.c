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
#include <string.h>
#include <assert.h>
#include <tiffio.h>

#include "ridgeutil.h"

RutSurface *
rut_surface_new (int rows, int cols) {
  RutSurface *result = malloc (sizeof(RutSurface));
  size_t len = rows * cols * sizeof (float);
  result->rows = rows;
  result->cols = cols;
  result->rowstep = cols;
  result->colstep = 1;
  result->is_view = 0;
  result->data = rut_multiproc_malloc (len);
  return result;
}

RutSurface *
rut_surface_new_like (RutSurface *s) {
  return rut_surface_new (s->rows, s->cols);
}

void
rut_surface_destroy (RutSurface *s)
{
  if (!s) return;
  if (!s->is_view) {
    rut_multiproc_free (s->data);
  }
  free (s);
}

RutSurface *
rut_surface_from_tiff (const char *filename)
{
  TIFF *tif = NULL;
  uint32 width, length, rows_per_strip;
  uint16 sample_format, bits_per_sample, samples_per_pixel;
  RutSurface *result = NULL;
  char *buffer = NULL;
  int i, j, row;

  /* Open TIFF file */
  tif = TIFFOpen (filename, "rb");
  if (!tif) {
    goto loadfail;
  }

  /* Get standard tags */
  if (!(TIFFGetField (tif, TIFFTAG_IMAGEWIDTH, &width) &&
        TIFFGetField (tif, TIFFTAG_IMAGELENGTH, &length) &&
        TIFFGetField (tif, TIFFTAG_SAMPLEFORMAT, &sample_format) &&
        TIFFGetField (tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample) &&
        TIFFGetField (tif, TIFFTAG_SAMPLESPERPIXEL, &samples_per_pixel))) {
    goto loadfail;
  }

  /* Validate compatibility */
  if (!TIFFGetField (tif, TIFFTAG_ROWSPERSTRIP, &rows_per_strip) ||
      (sample_format != SAMPLEFORMAT_IEEEFP) ||
      (bits_per_sample != 32) ||
      (samples_per_pixel != 1) ||
      TIFFIsTiled (tif)) {
    goto loadfail;
  }

  /* Create surface and allocate read buffer */
  result = rut_surface_new (length, width);
  buffer = malloc (TIFFStripSize (tif));

  /* Read data */
  for (i = 0, row = 0; i < TIFFNumberOfStrips (tif); i++) {
    int size = TIFFReadEncodedStrip (tif, i, buffer, TIFFStripSize (tif));
    int numrows = size/(width * 4);
    if (size % (width * 4) != 0) {
      /* Oops, not an integer number of rows! */
      goto loadfail;
    }

    for (j = 0; j < numrows; j++, row++) {
      char *src = buffer + j*width*4;
      float *dest = RUT_SURFACE_PTR (result, row, 0);
      memcpy (dest, src, width*4);
    }
  }

  /* Clean up */
  free (buffer);
  TIFFClose (tif);
  return result;

 loadfail:
  if (tif) TIFFClose (tif);
  if (buffer) free (buffer);
  if (result) rut_surface_destroy (result);
  fprintf (stderr, "Could not load TIFF from %s\n", filename);
  return NULL;
}

int
rut_surface_to_tiff (RutSurface *s, const char *filename)
{
  TIFF *tif = NULL;
  int rows_per_strip, num_strips, strip, row, i;
  int result = 1;
  char *buffer = NULL;

  assert (s);
  assert (filename);

  /* Open TIFF file */
  tif = TIFFOpen (filename, "wb");
  if (!tif) goto savefail;

  /* Set TIFF tags */
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, s->cols);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, s->rows);
  TIFFSetField (tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField (tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

  rows_per_strip = TIFFDefaultStripSize (tif, 0);
  TIFFSetField (tif, TIFFTAG_ROWSPERSTRIP, rows_per_strip);
  num_strips = TIFFNumberOfStrips (tif);

  /* Copy data into TIFF strips */
  buffer = malloc (TIFFStripSize (tif));

  for (row = 0, strip = 0; strip < num_strips; strip++) {
    int size = 0;
    for (i = 0; (i < rows_per_strip) && (row < s->rows); i++, row++) {
      float *src = RUT_SURFACE_PTR (s, row, 0);
      char *dest = buffer + i*s->cols*4;
      memcpy (dest, src, s->cols * 4);
      size += s->cols*4;
    }
    result = (TIFFWriteEncodedStrip (tif, strip, buffer, size) != -1);
    if (!result) {
      fprintf (stderr, "Could not write TIFF strip %i/%i to %s\n",
               strip+1, num_strips, filename);
    }
  }

  /* Clean up */
  free (buffer);
  TIFFClose (tif);
  return 1;

 savefail:
  if (tif) TIFFClose (tif);
  if (buffer) free (buffer);
  fprintf (stderr, "Could not save TIFF to %s\n", filename);
  return 0;
}

RutSurface *
rut_surface_new_view (RutSurface *s)
{
  assert (s);
  RutSurface *result = malloc (sizeof (RutSurface));
  result->rows = s->rows;
  result->cols = s->cols;
  result->rowstep = s->rowstep;
  result->colstep = s->colstep;
  result->is_view = 1;
  result->data = s->data;
  return result;
}

RutSurface *
rut_surface_new_row_view (RutSurface *s, int row)
{
  assert (s);
  assert (row >= 0 && row < s->rows);
  RutSurface *result = rut_surface_new_view (s);
  /* Set up single row view */
  result->data = RUT_SURFACE_PTR (result, row, 0);
  result->rows = 1;
  return result;
}

RutSurface *
rut_surface_new_column_view (RutSurface *s, int col)
{
  assert (s);
  assert (col >= 0 && col < s->cols);
  RutSurface *result = rut_surface_new_view (s);
  /* Set up single column view */
  result->data = RUT_SURFACE_PTR (result, 0, col);
  result->cols = 1;
  return result;
}

void
rut_surface_transpose (RutSurface *s)
{
  int tmp;
  assert (s);

  tmp = s->rows;
  s->rows = s->cols;
  s->cols = tmp;

  tmp = s->rowstep;
  s->rowstep = s->colstep;
  s->colstep = tmp;
}
