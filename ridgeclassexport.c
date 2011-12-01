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
#include <ctype.h>
#include <errno.h>

#include <ridgeio.h>

#define GETOPT_OPTIONS "h"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE OUTFILE1 OUTFILE2\n"
"\n"
"Split classified ridge data into per-class files.\n"
"\n"
"  -h              Display this message and exit\n"
"\n"
"Loads classified ridges from INFILE, and splits them into OUTFILE1 and\n"
"OUTFILE2 according to their classification.\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);
  exit (status);
}

int main (int argc, char **argv)
{
  int c;
  char *infile = NULL;
  char *outfiles[2] = {NULL, NULL};
  const uint8_t *classification = NULL;
  size_t classification_size = 0;
  RioData *data = NULL, *exdata[2] = {NULL, NULL};

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
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

  /* Get input and output filenames */
  if (argc - optind < 3) {
    fprintf (stderr, "ERROR: You must specify one input and two output filenames.\n\n");
    usage (argv[0], 1);
  }
  infile = argv[optind];
  outfiles[0] = argv[optind+1];
  outfiles[1] = argv[optind+2];

  /* Attempt to load input file */
  data = rio_data_from_file (infile);
  if (data == NULL) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Could not load ridge data from %s: %s\n",
             infile, msg);
    exit (2);
  }

  /* Check metadata */
  classification = (uint8_t *) rio_data_get_metadata (data,
                                                      RIO_KEY_IMAGE_CLASSIFICATION,
                                                      &classification_size);
  if (classification == NULL) {
    fprintf (stderr, "ERROR: %s does not contain classification metadata\n",
             infile);
    exit (3);
  }
  if (classification_size != rio_data_get_num_entries (data)) {
    fprintf (stderr ,"ERROR: %s contains invalid classification metadata\n",
             infile);
    exit (3);
  }

  /* Create output data */
  /* FIXME need to copy over *all* metadata, not just image
   * dimensions */
  for (int i = 0; i < 2; i++) {
    exdata[i] = rio_data_new (rio_data_get_type (data));
    uint32_t v;

    if (rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_ROWS, &v)) {
      rio_data_set_metadata_uint32 (exdata[i], RIO_KEY_IMAGE_ROWS, v);
    }
    if (rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_COLS, &v)) {
      rio_data_set_metadata_uint32 (exdata[i], RIO_KEY_IMAGE_COLS, v);
    }
  }

  /* Split data between files */
  switch (rio_data_get_type (data)) {
  case RIO_DATA_LINES:
    for (int i = 0; i < rio_data_get_num_entries (data); i++) {
      RioData *dest = exdata[classification[i] != 0];
      RioLine *l = rio_data_get_line (data, i);
      RioLine *m = rio_data_new_line (dest);
      for (int j = 0; j < rio_line_get_length (l); j++) {
        RioPoint *p = rio_line_new_point (m);
        *p = *rio_line_get_point (l, j);
      }
    }
    break;
  case RIO_DATA_SEGMENTS:
    for (int i = 0; i < rio_data_get_num_entries (data); i++) {
      RioData *dest = exdata[classification[i] != 0];
      RioSegment *s = rio_data_get_segment (data, i);
      RioSegment *t = rio_data_new_segment (dest);
      *rio_segment_get_start (t) = *rio_segment_get_start (s);
      *rio_segment_get_end (t) = *rio_segment_get_end (s);
    }
    break;
  case RIO_DATA_POINTS:
    for (int i = 0; i < rio_data_get_num_entries (data); i++) {
      RioData *dest = exdata[classification[i] != 0];
      *rio_data_new_point (dest) = *rio_data_get_point (data, i);
    }
    break;
  default:
    abort();
  }

  /* Write new files */
  for (int i = 0; i < 2; i++) {
    int status = rio_data_to_file (exdata[i], outfiles[i]);
    if (!status) {
      const char *msg = errno ? strerror (errno) : "Unexpected error";
      fprintf (stderr, "ERORR: Could not write ridge data to %s: %s\n",
               outfiles[i], msg);
      exit (4);
    }
  }
}
