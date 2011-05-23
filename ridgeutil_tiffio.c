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

int
rut_tiff_get_size (const char *filename, int *rows, int *cols)
{
  TIFF *tif = NULL;
  int result = 0;
  uint32 width, length;

  /* Open TIFF file */
  tif = TIFFOpen (filename, "rb");
  if (!tif) {
    goto sizefail;
  }

  /* Get tags */
  if (!(TIFFGetField (tif, TIFFTAG_IMAGEWIDTH, &width) &&
        TIFFGetField (tif, TIFFTAG_IMAGELENGTH, &length))) {
    goto sizefail;
  }

  *rows = (int) width;
  *cols = (int) length;
  result = 1; /* Success */

 sizefail:
  TIFFClose (tif);
  return result;
}
