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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

#include "ridgeutil.h"

#define GETOPT_OPTIONS "t:h"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE\n"
"\n"
"Generate a scale-space from a TIFF file.\n"
"\n"
"  -t SCALES       Specify a single scale or a range of scales\n"
"  -h              Display this message and exit\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);
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

  if (!(str2 && str3)) {
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

static char *
get_filename_for_scale (const char *orig_name, float scale)
{
  /* If 4 s.f. isn't enough, tough. */
  size_t result_size = 1024;
  char *result = malloc (result_size);
  snprintf (result, result_size, "%s.%.4g", orig_name, (double) scale);
  return result;
}

int
main (int argc, char **argv)
{
  char *filename;
  int n_scales = 0;
  float *scales = NULL;
  int c, status;

  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 't':
      status = parse_scales (optarg, &n_scales, &scales);
      if (!status) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -t option.\n\n",
                 optarg);
        usage (argv[0], 1);
      }
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

  /* Check that at least one scale was specified */
  if (n_scales < 1) {
    fprintf (stderr, "ERROR: You must specify at least one scale.\n\n");
    usage (argv[0], 1);
  }

  /* Load TIFF */
  RutSurface *image = rut_surface_from_tiff (filename);
  if (!image) return 2; /* Should have already printed a message */

  /* Compute scale-space */
  RutScaleSpace *sspace =
    rut_scale_space_generate_mp (image, n_scales, scales);

  /* Export results */
  for (int i = 0; i < sspace->n_scales; i++) {
    char *outfile = get_filename_for_scale (filename, sspace->scales[i]);
    printf ("%s\n", outfile);
    RutSurface *simage =
      rut_scale_space_get_surface (sspace, RUT_SCALE_SPACE_SCALE, i);
    status = rut_surface_to_tiff (simage, outfile);
    if (!status) return 3; /* Should have already printed a message */
    rut_surface_destroy (simage);
    free (outfile);
  }
}
