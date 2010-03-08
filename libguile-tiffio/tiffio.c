/*  libguile-tiffio: Guile interface to libtiff
 *  Copyright (C) 2010 Peter TB Brett <peter@peter-b.co.uk>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/* At the moment this is pretty rudimentary. */

/* We want to use vasprintf(3) */
#define _GNU_SOURCE

#include <stdarg.h>
#include <stdio.h>

#include <libguile.h>
#include <tiffio.h>



static void
handle_tiff_err (const char *module, const char *fmt, va_list ap)
{
  char *message, *full_message;
  int printed;
  SCM s_message, s_module;

  /* Format error message */
  printed = vasprintf (&message, fmt, ap);
  if (printed == -1) {
    scm_memory_error (__func__);
  }
  if (module != NULL) {
    printed = asprintf (&full_message, "%s: %s", module, message);
    if (printed == -1) {
      scm_memory_error (__func__);
    }
    free (message);
  } else {
    full_message = message;
  }

  /* Convert error message to SCM */
  s_message = scm_take_locale_string (full_message);

  /* Emit error */
  scm_error_scm (scm_misc_error_key, /* key */
                 SCM_BOOL_F,         /* subr */
                 s_message,          /* message */
                 SCM_EOL,            /* args */
                 SCM_BOOL_F);        /* rest */

  scm_remember_upto_here_1 (s_message);
}

/* Clean up open TIFF file pointer on non-local exit */
static void
cleanup_tiff_ptr (TIFF *tif)
{
  TIFFClose (tif);
}

static void
get_field_or_fail (TIFF *tif, ttag_t tag, const char* tag_str, ...)
{
  int result;
  va_list ap;

  va_start(ap, tag_str);
  result = TIFFVGetField(tif, tag, ap);
  va_end(ap);

  if (!result) {
    SCM args = scm_list_1 (scm_from_locale_string (tag_str));
    scm_misc_error (NULL,                        /* subr */
                    "Missing required tag: ~A",  /* message */
                    args);                       /* rest */
    scm_remember_upto_here_1 (args);
  }
}

/* Load a TIFF file */
SCM_DEFINE (load_tiff, "%load-tiff", 1, 0, 0,
            (SCM s_filename),
            "Load a TIFF image.")
{
  TIFF *tif = NULL;
  char *filename;
  uint32 width, length, rows_per_strip;
  uint16 sample_format, bits_per_sample, samples_per_pixel;
  SCM result;
  scm_t_array_handle handle;
  scm_t_array_dim *dims;
  float *data;
  float *buffer;
  int i,j,row,col;

  scm_dynwind_begin (0);

  filename = scm_to_locale_string (s_filename);
  scm_dynwind_free (filename);


  /* Attempt to open TIFF file */
  tif = TIFFOpen (filename, "rb");
  if (tif == NULL) {
    scm_misc_error (NULL, "Failed to open TIFF file ~A.",
                    scm_list_1 (s_filename));
  }
  scm_dynwind_unwind_handler ((void (*)(void *))cleanup_tiff_ptr,
                              tif, SCM_F_WIND_EXPLICITLY);

  /* Get standard tags */
#define GET_FIELD(tag, dest) get_field_or_fail(tif, tag, #tag, &dest)
  GET_FIELD (TIFFTAG_IMAGEWIDTH, width);
  GET_FIELD (TIFFTAG_IMAGELENGTH, length);
  GET_FIELD (TIFFTAG_SAMPLEFORMAT, sample_format);
  GET_FIELD (TIFFTAG_BITSPERSAMPLE, bits_per_sample);
  GET_FIELD (TIFFTAG_SAMPLESPERPIXEL, samples_per_pixel);
#undef GET_FIELD

  /* Validate compatibility */
  if (!TIFFGetField (tif, TIFFTAG_ROWSPERSTRIP, &rows_per_strip) ||
      (sample_format != SAMPLEFORMAT_IEEEFP) ||
      (bits_per_sample != 32) ||
      (samples_per_pixel != 1) ||
      TIFFIsTiled (tif)) {

    scm_misc_error (NULL, "Unsupported TIFF format", SCM_EOL);
  }

  /* Create output array -- FIXME don't zero-fill
   *
   * First dimension is number of rows, second number of columns (same
   * as MATLAB). */
  result =
    scm_make_typed_array (scm_from_locale_symbol ("f32"),
                          scm_variable_ref (scm_c_lookup ("*unspecified*")),
                          scm_list_2 (scm_from_int (length),
                                      scm_from_int (width)));

  /* Reserve array handle & make sure it's properly cleaned up */
  scm_array_get_handle (result, &handle);
  scm_dynwind_unwind_handler ((void (*)(void *))scm_array_handle_release,
                              &handle,
                              SCM_F_WIND_EXPLICITLY);
  data = scm_array_handle_f32_writable_elements (&handle);
  dims = scm_array_handle_dims (&handle);

  /* Allocate read buffer */
  buffer = malloc (TIFFStripSize (tif));
  if (buffer == NULL) {
    scm_memory_error (__func__);
  }
  scm_dynwind_free (buffer);

  /* Read data */
  for (i = 0, row = 0; i < TIFFNumberOfStrips (tif); i++) {
    int size = TIFFReadEncodedStrip (tif, i, buffer, TIFFStripSize (tif));
    int numrows = size/(width*4);
    if (size % (width*4) != 0) {
      /* Oops, not an integer number of rows! */
      scm_misc_error (NULL, "Strip contains invalid number of rows", SCM_EOL);
    }

    for (j = 0; j < numrows; j++, row++) {
      for (col = 0; col < width; col++) {
        float *dest = (data +
                       (row - dims[0].lbnd)*dims[0].inc +
                       (col - dims[1].lbnd)*dims[1].inc);
        float *src = buffer + j*width + col;
        *dest = *src;
      }
    }
  }

  scm_dynwind_end ();

  return result;
}


/* Save a TIFF file */
SCM_DEFINE (save_tiff, "%save-tiff", 2, 0, 0,
            (SCM array, SCM s_filename),
            "Save a TIFF image.")
{
  TIFF *tif = NULL;
  int rows_per_strip, num_strips, strip, i;
  int length, width;
  int row, col;
  float *buffer = NULL;
  char *filename;
  scm_t_array_handle handle;
  scm_t_array_dim *dims;
  const float *data;

  scm_dynwind_begin (0);

  if (!scm_is_typed_array(array, scm_from_locale_symbol ("f32"))
      || (scm_c_array_rank (array) != 2)) {
    scm_wrong_type_arg (NULL, SCM_ARG1, array);
  }

  /* Reserve array handle & make sure it's properly cleaned up */
  scm_array_get_handle (array, &handle);
  scm_dynwind_unwind_handler ((void (*)(void *))scm_array_handle_release,
                              &handle,
                              SCM_F_WIND_EXPLICITLY);
  data = scm_array_handle_f32_elements (&handle);
  dims = scm_array_handle_dims (&handle);

  length = (dims[0].ubnd - dims[0].lbnd + 1);
  width = (dims[1].ubnd - dims[1].lbnd + 1);

  /* Get filename */
  filename = scm_to_locale_string (s_filename);
  scm_dynwind_free (filename);

  /* Open TIFF file */
  tif = TIFFOpen (filename, "wb");
  if (tif == NULL) {
    scm_misc_error (NULL, "Failed to open TIFF file ~A.",
                    scm_list_1 (s_filename));
  }
  scm_dynwind_unwind_handler ((void (*)(void *))cleanup_tiff_ptr,
                              tif, SCM_F_WIND_EXPLICITLY);

  /* Set tags */
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, length);
  TIFFSetField (tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);

  rows_per_strip = TIFFDefaultStripSize (tif, 0);
  TIFFSetField (tif, TIFFTAG_ROWSPERSTRIP, rows_per_strip);
  num_strips = TIFFNumberOfStrips (tif);

  /* Allocate write buffer */
  buffer = malloc (TIFFStripSize (tif));
  if (buffer == NULL) {
    scm_memory_error (__func__);
  }
  scm_dynwind_free (buffer);

  row = 0;
  for (strip = 0; strip < num_strips; strip++) {
    int size = 0;
    int result;

    for (i = 0; (i < rows_per_strip) && (row < length); i++, row++) {
      for (col = 0; col < width; col++) {
        const float *src = (data +
                      (row - dims[0].lbnd)*dims[0].inc +
                      (col - dims[1].lbnd)*dims[1].inc);
        float *dest = buffer + i*width + col;
        *dest = *src;
      }
      size += width*4;
    }

    result = TIFFWriteEncodedStrip (tif, strip, buffer, size);
    if (result == -1) {
      scm_misc_error (NULL, "Failed to write strip ~A",
                      scm_list_1 (scm_from_int (strip)));
    }
  }

  scm_dynwind_end ();

  return SCM_BOOL_T;
}


void
init_tiffio ()
{
  TIFFSetErrorHandler (handle_tiff_err);

  #include "tiffio.x"
}
