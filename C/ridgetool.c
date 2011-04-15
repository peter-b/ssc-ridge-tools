#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <assert.h>

#include "ridgetool.h"

enum {
  MODE_SEGMENTS,
  MODE_POINTS,
  MODE_LINES,
};

#define GETOPT_OPTIONS "slpt:m:j:h"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE [OUTFILE]\n"
"\n"
"Scale-space image ridge extraction tool.\n"
"\n"
"  -s              Extract ridge segments [default mode]\n"
"  -l              Extract ridge lines\n"
"  -p              Extract ridge points\n"
"  -t SCALES       Specify a single scale or a range of scales\n"
"  -m NORM         Strength metric to use (A, M or N) [default N]\n"
"  -j THREADS      Number of multiprocessing threads to use [default %i]\n"
"  -h              Display this message and exit\n"
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
  RutSurface *image = NULL;
  RutSurface *Lp = NULL, *Lpp = NULL, *RnormL = NULL;
  RidgePointsSS *ridges = NULL;
  RidgeLinesSS *lines = NULL;
  Filter *filt;
  int c, i;
  int n_scales = 0;
  float *scales = NULL;;
  int show_result = 0;
  int status;
  int mode = MODE_SEGMENTS;
  int metric = METRICS_NNORM;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
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
    case 'v':
      show_result = 1;
      break;
    case 'h':
      usage (argv[0], 0);
      break;
    case '?':
      if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
        fprintf (stderr, "ERROR: -%c option requires an argument.\n\n", optopt);
      } else if (isprint (optopt)) {
        fprintf (stderr, "ERROR: Unknown option -%c.\n\n", optopt);
      } else {
        fprintf (stderr, "ERROR: Unknown option character '\\x%x'.\n\n",
                 optopt);
      }
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
  image = rut_surface_from_tiff (filename);
  if (!image) return 2; /* Should have already output a message */

  /* Create single-scale metrics for lowest scale requested. */
  filt = filter_new_gaussian (scales[0]);
  if (filt) {
    MP_filter (filt, image, image, FILTER_FLAG_ROWS | FILTER_FLAG_COLS);
    filter_destroy (filt);
  }

  Lp = rut_surface_new_like (image);
  Lpp = rut_surface_new_like (image);
  RnormL = rut_surface_new_like (image);
  MP_metrics_SS (image, scales[0], METRICS_NNORM, Lp, Lpp, RnormL);

  /* Free up image to save memory */
  rut_surface_destroy (image);

  /* Find ridge points */
  ridges = ridge_points_SS_new_for_surface (RnormL);
  MP_ridge_points_SS (ridges, Lp, Lpp);

  rut_surface_destroy (Lp);
  rut_surface_destroy (Lpp);

  if (mode == MODE_LINES) {
    /* Find ridge lines */
    lines = ridge_lines_SS_new_for_surface (RnormL);
    MP_ridge_lines_SS_build (lines, ridges);
  }

  /* Re-load image */
  image = rut_surface_from_tiff (filename);

  /* Generate output */
  if (out_filename) {
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
