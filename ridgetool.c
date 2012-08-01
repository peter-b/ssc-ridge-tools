/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2010-2011 Peter Brett <p.brett@surrey.ac.uk>
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#include <getopt.h>

#include "ridgetool.h"

enum {
  MODE_SEGMENTS,
  MODE_POINTS,
  MODE_LINES,
};

#define GETOPT_OPTIONS "slpt:m:j:i:vh"

struct option long_options[] =
  {
    {"segments", 0, 0, 's'},
    {"lines", 0, 0, 'l'},
    {"points", 0, 0, 'p'},
    {"scales", 1, 0, 't'},
    {"metric", 1, 0, 'm'},
    {"threads", 1, 0, 'j'},
    {"nan", 1, 0, 'i'},
    {"help", 0, 0, 'h'},

    {"verbose", 0, 0, 'v'},
    {"dump-L", 1, 0, '1'},
    {"dump-Lp", 1, 0, '2'},
    {"dump-Lpp", 1, 0, '3'},
    {"dump-RnormL", 1, 0, '4'},

    {0, 0, 0, 0} /* Guard */
  };

int verbose_mode;

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE [OUTFILE]\n"
"\n"
"Scale-space image ridge extraction tool.\n"
"\n"
"Options:\n"
"  -s, --segments  Extract ridge segments [default mode]\n"
"  -l, --lines     Extract ridge lines\n"
"  -p, --points    Extract ridge points\n"
"  -t, --scales=SCALES  Specify a single scale or a range of scales\n"
"  -m, --metric=NORM    Strength metric to use (A, M or N) [default N]\n"
"  -j, --threads=NUM  Number of parallel threads to use [default %i]\n"
"  -i, --nan=VAL   Set non-finite input values to VAL [default 0]\n"
"  -h, --help      Display this message and exit\n"
"\n"
"Debugging options:\n"
"  -v, --verbose   Enable debugging output\n"
"  --dump-L=FILE   Dump scale-space representation to FILE\n"
"  --dump-Lp=FILE  Dump Lp metric to FILE\n"
"  --dump-Lpp=FILE    Dump Lpp metric to FILE\n"
"  --dump-R=FILE   Dump strength metric to FILE\n"
"\n"
"Extracts ridges from INFILE, which should be a single-channel 32-bit\n"
"IEEE floating point TIFF file.  Optionally, outputs extracted ridge\n"
"data to OUTFILE.\n"
"\n"
"Please report bugs to %s.\n",
name, rut_multiproc_threads, PACKAGE_BUGREPORT);

  exit (status);
}

static int
parse_scales (char *scale_arg, int *n_scales, float **scales)
{
  char *str1, *str2, *str3;
  float start, step, end, v;
  int status = 1, n, i;
  assert (scale_arg);

  /* Split argument by ':' characters */
  str3 = strdup (scale_arg);
  str1 = strsep (&str3, ":");
  str2 = strsep (&str3, ":");

  if (str2 == NULL) {
    /* Assume a single scale has been specified */
    *scales = realloc (*scales, ++(*n_scales) * sizeof (float));
    status = sscanf (str1, "%f", &(*scales)[*n_scales - 1]);
    free (str1);
    return (status == 1);
  }

  /* Parse scale spec components */
  status &= (sscanf (str1, "%f", &start) == 1);
  status &= (sscanf (str2, "%f", &step) == 1);
  status &= (sscanf (str3, "%f", &end) == 1);
  free (str1);

  if (!status) return 0;

  /* Check they make sense */
  if (step < 0) {
    step = -step;
    v = end;
    end = start;
    start = v;
  }

  if ((end < 0) || (start < 0) || (end < start)) return 0;

  n = lrintf (floorf ((end - start) / step) + 1);
  *n_scales += n;
  *scales = realloc (*scales, *n_scales * sizeof (float));
  for (i = 0; i < n; i++) {
    (*scales)[*n_scales - i - 1] = start + (n - i - 1)*step;
  }

  return 1;
}

static int
scale_compar (const void *a, const void *b)
{
  float x = *(const float *) a;
  float y = *(const float *) b;
  if (x == y) return 0;
  if (x < y) return -1;
  return 1;
}

int
main (int argc, char **argv)
{
  char *filename, *out_filename = NULL;
  char *L_dumpfile = NULL, *Lp_dumpfile = NULL,
    *Lpp_dumpfile = NULL, *RnormL_dumpfile = NULL;
  RutSurface *image = NULL;
  RutSurface *Lp = NULL, *Lpp = NULL, *RnormL = NULL;
  RidgePointsSS *ridges = NULL;
  RidgeLinesSS *lines = NULL;
  RutFilter *filt;
  int c, i, j;
  int n_scales = 0;
  float *scales = NULL;
  float nan_default = 0;
  int status;
  int mode = MODE_SEGMENTS;
  int metric = METRICS_NNORM;

  /* Quiet by default */
  verbose_mode = 0;

  /* Parse command-line arguments */
  while (1) {
    c = getopt_long (argc, argv, GETOPT_OPTIONS, long_options, NULL);
    if (c == -1) break;

    switch (c) {
    case 's':
      mode = MODE_SEGMENTS;
      break;
    case 'l':
      mode = MODE_LINES;
      break;
    case 'p':
      mode = MODE_POINTS;
      break;
    case 't':
      status = parse_scales (optarg, &n_scales, &scales);
      if (!status) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -t option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
      break;
    case 'm':
      switch (optarg[0]) {
      case 'A': metric = METRICS_ANORM; break;
      case 'M': metric = METRICS_MNORM; break;
      case 'N': metric = METRICS_NNORM; break;
      default:
        fprintf (stderr, "ERROR: Bad argument '%s' to -m option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
      break;
    case 'j':
      status = sscanf (optarg, "%i", &rut_multiproc_threads);
      if (status != 1) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -j option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
      break;
    case 'i':
      status = sscanf (optarg, "%f", &nan_default);
      if (status != 1) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -i option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
    case 'h':
      usage (argv[0], 0);
      break;

    case 'v':
      verbose_mode = 1;
      break;
    case '1':
      L_dumpfile = optarg;
      break;
    case '2':
      Lp_dumpfile = optarg;
      break;
    case '3':
      Lpp_dumpfile = optarg;
      break;
    case '4':
      RnormL_dumpfile = optarg;
      break;

    case '?':
      usage (argv[0], 1);
    default:
      abort ();
    }
  }

  /* Get filename */
  if (argc - optind == 0) {
    fprintf (stderr, "ERROR: You must specify an input filename.\n\n");
    usage (argv[0], 1);
  }
  filename = argv[optind];

  /* Get output filename */
  if (argc - optind > 1) {
    out_filename = argv[optind+1];
  }

  /* Sort scales and remove duplicates. If no scales have been
   * specified, just use 0 as the scale to extract at. */
  if (scales == NULL) {
    scales = realloc (scales, sizeof (float));
    scales[0] = 0;
  }
  qsort (scales, n_scales, sizeof (float), scale_compar);

  for (c = 1, i = 0; c < n_scales; c++) {
    if (scales[i] == scales[c]) continue;
    scales[++i] = scales[c];
  }
  n_scales -= (c-i-1);

  /* Load image */
  debug_printf ("Loading image from %s.\n", filename);
  image = rut_surface_from_tiff (filename);
  if (!image) return 2; /* Should have already output a message */

  /* Set non-finite values to default value */
  debug_printf ("Setting non-finite values to %f.\n", nan_default);
  for (i = 0; i < image->rows; i++) {
    for (j = 0; j < image->cols; j++) {
      if (!isfinite (RUT_SURFACE_REF (image, i, j))) {
        RUT_SURFACE_REF (image, i, j) = nan_default;
      }
    }
  }

  /* Create single-scale metrics for lowest scale requested. */
  debug_printf ("Filtering to scale %f\n", scales[0]);
  filt = rut_filter_new_gaussian (scales[0]);
  if (filt) {
    rut_filter_surface_mp (filt, image, image,
                           RUT_SURFACE_ROWS | RUT_SURFACE_COLS);
    rut_filter_destroy (filt);
  }

  /* Dump scale-space representation if requested. */
  if (L_dumpfile) {
    debug_printf ("Dumping filtered image to %s.\n", L_dumpfile);
    /* Deliberately ignore return status; message will already have
     * been output. */
    rut_surface_to_tiff (image, L_dumpfile);
  }

  debug_printf ("Calculating metrics\n");
  Lp = rut_surface_new_like (image);
  Lpp = rut_surface_new_like (image);
  RnormL = rut_surface_new_like (image);
  MP_metrics_SS (image, scales[0], METRICS_NNORM, Lp, Lpp, RnormL);

  /* Free up image to save memory */
  rut_surface_destroy (image);

  /* Dump metrics if requested. */
  /* Deliberately ignore return status; message will already have
   * been output. */
  if (Lp_dumpfile) {
    debug_printf ("Dumping Lp metric to %s.\n", Lp_dumpfile);
    rut_surface_to_tiff (Lp, Lp_dumpfile);
  }
  if (Lpp_dumpfile) {
    debug_printf ("Dumping Lpp metric to %s.\n", Lpp_dumpfile);
    rut_surface_to_tiff (Lpp, Lpp_dumpfile);
  }
  if (RnormL_dumpfile) {
    debug_printf ("Dumping R metric to %s.\n", RnormL_dumpfile);
    rut_surface_to_tiff (RnormL, RnormL_dumpfile);
  }


  /* Find ridge points */
  debug_printf ("Finding ridge points\n");
  ridges = ridge_points_SS_new_for_surface (RnormL);
  MP_ridge_points_SS (ridges, Lp, Lpp);

  rut_surface_destroy (Lp);
  rut_surface_destroy (Lpp);

  if (mode == MODE_LINES) {
    /* Find ridge lines */
    debug_printf ("Building lines\n");
    lines = ridge_lines_SS_new_for_surface (RnormL);
    MP_ridge_lines_SS_build (lines, ridges);
  }

  /* Re-load image */
  debug_printf ("Re-loading image from %s.\n", filename);
  image = rut_surface_from_tiff (filename);

  /* Generate output */
  if (out_filename) {
    debug_printf ("Generating output to %s.\n", out_filename);
    switch (mode) {
    case MODE_POINTS:
      status = export_points (ridges, image, RnormL, out_filename);
      break;
    case MODE_SEGMENTS:
      status = export_segments (ridges, image, RnormL, out_filename);
      break;
    case MODE_LINES:
      status = export_lines (lines, ridges, image, RnormL, out_filename);
      break;
    default:
      abort ();
    }
    if (!status) {
      const char *msg = errno ? strerror (errno) : "Unexpected error";
      fprintf (stderr, "ERROR: Could not save ridge data to %s: %s\n",
               out_filename, msg);
      exit (3);
    }
  }

  free (scales);
  rut_surface_destroy (RnormL);

  ridge_points_SS_destroy (ridges);
  ridge_lines_SS_destroy (lines);

  return 0;
}

void
debug_printf (char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  if (verbose_mode) vfprintf (stderr, format, ap);
  va_end (ap);
}
