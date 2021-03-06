/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2012  Peter Brett <p.brett@surrey.ac.uk>
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
#include <arpa/inet.h>

#include <ridgeio.h>

#define GETOPT_OPTIONS "sdh"

enum {
  MODE_SUMMARY,
  MODE_DUMP,
};

typedef struct _ClassificationData ClassificationData;

struct _ClassificationData {
  size_t num_entries;
  uint8_t *classification;
  float *likelihood;
};

static void
usage (char *name, int status)
{
  printf (
"Usage: %s [-s | -d] FILE [FILE ...]\n"
"\n"
"Show statistics of ridge classification data.\n"
"\n"
"  -s              Classification summary mode (default)\n"
"  -d              Print classification data mode\n"
"  -h              Display this message and exit\n"
"\n"
"Loads classified ridges from one or more input files, and print some\n"
"statistics about the classification.  If more than one input file was\n"
"specified in summary (-s) mode, the first file specified is taken as a\n"
"reference classification, and for each of the remaining files\n"
"classification error information is printed.\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);
  exit (status);
}

static ClassificationData *
load_classification (const char *filename)
{
  RioData *data;
  const uint8_t *buf;
  size_t buf_size;
  ClassificationData *result = malloc(sizeof(ClassificationData));

  /* Attempt to load input file */
  data = rio_data_from_file (filename);
  if (data == NULL) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Could not load ridge data from %s: %s\n",
             filename, msg);
    exit (2);
  }

  /* Check classification metadata */
  buf = (uint8_t *) rio_data_get_metadata (data, RIO_KEY_IMAGE_CLASSIFICATION,
                                           &buf_size);
  if (buf == NULL) {
    fprintf (stderr, "ERROR: %s does not contain classification metadata\n",
             filename);
    exit (3);
  }
  if (buf_size != rio_data_get_num_entries (data)) {
    fprintf (stderr ,"ERROR: %s contains invalid classification metadata\n",
             filename);
    exit (3);
  }

  /* Copy classification data */
  result->num_entries = rio_data_get_num_entries (data);
  result->classification = malloc (buf_size * sizeof (uint8_t));
  memcpy (result->classification, buf, buf_size);

  /* Check likelihood metadata */
  result->likelihood = NULL;
  buf = (uint8_t *) rio_data_get_metadata (data, RIO_KEY_IMAGE_CLASS_LIKELIHOOD,
                                           &buf_size);
  if (buf != NULL) {
    if (buf_size != rio_data_get_num_entries (data) * sizeof (float)) {
      fprintf (stderr ,"ERROR: %s contains invalid classification metadata\n",
               filename);
      exit (3);
    }

    result->likelihood = malloc (buf_size * sizeof (float));
    memcpy (result->likelihood, buf, buf_size);
  }

  rio_data_destroy (data);
  return result;
}

static void
print_basic_stats (const uint8_t *classification, size_t N)
{
  size_t A = 0, B = 0;
  for (int i = 0; i < N; i++) {
    A += (classification[i] == 0);
    B += (classification[i] != 0);
  }

  printf ("Class A: %zi / %zi (%.2f%%)\n", A, N, 100.0*A/N);
  printf ("Class B: %zi / %zi (%.2f%%)\n", B, N, 100.0*B/N);
}

static void
print_diff_stats (const uint8_t *classification,
                  const uint8_t *reference, size_t N)
{
  size_t AB = 0, BA = 0;
  for (int i = 0; i < N; i++) {
    AB += (classification[i] != 0 && reference[i] == 0);
    BA += (classification[i] == 0 && reference[i] != 0);
  }

  printf ("Error A->B: %zi / %zi (%.2f%%)\n", AB, N, 100.0*AB/N);
  printf ("Error B->A: %zi / %zi (%.2f%%)\n", BA, N, 100.0*BA/N);
}

int main (int argc, char **argv)
{
  int c, mode = MODE_SUMMARY;
  char *infile = NULL;
  ClassificationData *ref = NULL;
  ClassificationData *data = NULL;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 's':
      mode = MODE_SUMMARY;
      break;
    case 'd':
      mode = MODE_DUMP;
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

  /* Check that we got at least one filename */
  if (argc - optind < 1) {
    fprintf (stderr, "ERROR: You must specify at least one input filename.\n\n");
    usage (argv[0], 1);
  }

  switch (mode) {

  case MODE_SUMMARY:
    while (argc - optind) {
      infile = argv[optind++];

      data = load_classification (infile);
      printf ("File: %s\n\n", infile);
      print_basic_stats (data->classification, data->num_entries);
      printf ("\n");

      if (ref != NULL) {
        if (data->num_entries != ref->num_entries) {
          fprintf (stderr, "ERROR: Input files contain different numbers of elements.\n\n");
          exit (4);
        }
        print_diff_stats (data->classification, ref->classification,
                          ref->num_entries);
        free (data->classification);
        free (data->likelihood);
        free (data);
      } else {
        ref = data;
      }
    }
    break;

  case MODE_DUMP:
    if (argc - optind > 1) {
      fprintf (stderr, "Warning: Only the first input file specified is used in dump mode.\n\n");
    }
    data = load_classification (argv[optind]);
    for (int i = 0; i < data->num_entries; i++) {
      if (data->likelihood) {
        printf ("%i\t%e\n", (int) data->classification[i],
                rio_ntohf (data->likelihood[i]));
      } else {
        printf ("%i\n", (int) data->classification[i]);
      }
    }
    break;
  default:
    abort ();
  }

  return 0;
}
