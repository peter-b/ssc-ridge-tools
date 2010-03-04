#include <cv.h>
#include <tiffio.h>
#include <stdio.h>

#include "floattiff.h"

CvMat *
load_float_tiff (const char *filename)
{
  TIFF *tif;
  uint32 width, length, rows_per_strip;
  uint16 sample_format, bits_per_sample, samples_per_pixel;
  CvMat *result;
  char *buffer;
  int i, j, row;

  /* Open TIFF file */
  tif = TIFFOpen (filename, "rb");
  if (!tif) {
    return NULL;
  }

  /* Get standard tags */
  if (!(TIFFGetField (tif, TIFFTAG_IMAGEWIDTH, &width) &&
        TIFFGetField (tif, TIFFTAG_IMAGELENGTH, &length) &&
        TIFFGetField (tif, TIFFTAG_SAMPLEFORMAT, &sample_format) &&
        TIFFGetField (tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample) &&
        TIFFGetField (tif, TIFFTAG_SAMPLESPERPIXEL, &samples_per_pixel))) {
    fprintf (stderr, "TIF missing necessary tags\n");
    TIFFClose (tif);
    return NULL;
  }

  /* Validate compatibility */
  if (!TIFFGetField (tif, TIFFTAG_ROWSPERSTRIP, &rows_per_strip) ||
      (sample_format != SAMPLEFORMAT_IEEEFP) ||
      (bits_per_sample != 32) ||
      (samples_per_pixel != 1) ||
      TIFFIsTiled (tif)) {
    fprintf (stderr, "%i\n", sample_format);
    fprintf (stderr, "Unsupported TIF format\n");
    TIFFClose (tif);
    return NULL;
  }

  /* Create output matrix */
  result = cvCreateMat (width, length, CV_32FC1);
  if (result == NULL) {
    fprintf (stderr, "Failed to create matrix!\n");
    exit (1);
  }

  /* Allocate read buffer */
  buffer = malloc (TIFFStripSize (tif));
  if (buffer == NULL) {
    fprintf (stderr, "Failed to allocate buffer!\n");
    exit (1);
  }

  /* Read data */
  for (i = 0, row = 0; i < TIFFNumberOfStrips (tif); i++) {
    int size = TIFFReadEncodedStrip (tif, i, buffer, TIFFStripSize (tif));
    int numrows = size/(width*4);
    if (size % (width*4) != 0) {
      /* Oops, not an integer number of rows! */
      fprintf (stderr, "Strip size error.\n");
      free (buffer);
      cvReleaseMat (&result);
      TIFFClose (tif);
      return NULL;
    }

    for (j = 0; j < numrows; j++, row++) {
      char *src = buffer + j*width*4;
      char *dest = result->data.ptr + row*result->step;
      memcpy (dest, src, width*4);
    }
  }
  free (buffer);
  TIFFClose (tif);
  return (result);
}

int
save_float_tiff (CvMat *mat, const char *filename)
{
  TIFF *tif = NULL;
  int rows_per_strip, num_strips, strip, row, i;
  int result = 1;
  char *buffer = NULL;

  if (CV_MAT_TYPE(mat->type) != CV_32FC1) {
    fprintf (stderr, "Bad matrix type.\n");
    return 0;
  }

  /* Open TIFF file */
  tif = TIFFOpen (filename, "wb");
  if (!tif) {
    return 0;
  }

  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, mat->cols);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, mat->rows);
  TIFFSetField (tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);

  rows_per_strip = TIFFDefaultStripSize (tif, 0);
  TIFFSetField (tif, TIFFTAG_ROWSPERSTRIP, rows_per_strip);
  num_strips = TIFFNumberOfStrips (tif);

  buffer = malloc (TIFFStripSize (tif));
  if (buffer == NULL) {
    fprintf (stderr, "Failed to allocate buffer!\n");
    exit (1);
  }
  for (strip = 0; strip < num_strips; strip++) {
    int size = 0;
    for (i = 0; (i < rows_per_strip) && (row < mat->rows); i++, row++) {
      char *src = mat->data.ptr + row*mat->step;
      char *dest = buffer + i*mat->cols*4;
      memcpy (dest, src, mat->cols*4);
      size += mat->cols*4;
    }
    result = (TIFFWriteEncodedStrip (tif, strip, buffer, size) != -1);
    if (!result) break;
  }

  free (buffer);
  TIFFClose (tif);
  return result;
}
