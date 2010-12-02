#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>
#include <cairo.h>
#include <gtk/gtk.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"

#define GETOPT_OPTIONS "s:t:M:vh"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... FILENAME\n"
"\n"
"Scale-space image ridge extraction tool.\n"
"\n"
"  -s SCALES       Specify a single scale or a range of scales\n"
"  -t THREADS      Number of multiprocessing threads to use [default %i]\n"
"  -M FILENAME     Output mask file showing locations of ridges\n"
"  -v              Display result\n"
"  -h              Display this message and exit\n"
"\n"
"Please report bugs to p.brett@surrey.ac.uk\n",
name, multiproc_threads);

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
  char *filename, *mask_filename = NULL;;
  Surface *image = NULL;
  Surface *Lp = NULL, *Lpp = NULL, *RnormL = NULL;
  Surface *mask = NULL;
  Filter *filt;
  int c, i;
  int n_scales = 0;
  float *scales = NULL;;
  int show_result = 0;
  int status;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 's':
      status = parse_scales (optarg, &n_scales, &scales);
      if (!status) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -s option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
      break;
    case 't':
      status = sscanf (optarg, "%i", &multiproc_threads);
      if (status != 1) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -t option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
      break;
    case 'M':
      mask_filename = optarg;
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
  if (argc - optind < 1) {
    fprintf (stderr, "ERROR: You must specify an input filename.\n\n");
    usage (argv[0], 1);
  }
  filename = argv[optind];

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

  /* Initialise GTK and load image */
  gtk_init (&argc, &argv);
  image = surface_from_tiff (filename);

  /* Create single-scale mask for lowest scale requested. */
  filt = filter_new_gaussian (scales[0]);
  if (filt) {
    MP_filter (filt, image, image, FILTER_FLAG_ROWS | FILTER_FLAG_COLS);
    filter_destroy (filt);
  }

  MP_metrics_SS (image, scales[0], METRICS_NNORM, &Lp, &Lpp, &RnormL);

  if (mask_filename != NULL) {
    MP_ridge_mask_SS (Lp, Lpp, &mask);
    surface_to_tiff (mask, mask_filename);
  }

  if (show_result) {
    MP_ridge_mask_SS (Lp, Lpp, &mask);
    show_surface_dialog (mask, 1);
  }

  free (scales);
  surface_destroy (image);
  surface_destroy (Lp);
  surface_destroy (Lpp);
  surface_destroy (RnormL);
  surface_destroy (mask);

  return 0;
}
