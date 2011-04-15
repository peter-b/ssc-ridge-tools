#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "ridgeconv.h"

enum {
  CONV_NONE,
  CONV_SVG,
  CONV_TIFF,
  CONV_PNG,
  CONV_CSV,
  CONV_PDF,
};

#define GETOPT_OPTIONS "sdcpth"

struct _ConvSuffix {
  const char *suffix;
  int conv;
};

struct _ConvSuffix conv_suffixes[] = {
  {"svg", CONV_SVG},
  {"tif", CONV_TIFF},
  {"tiff", CONV_TIFF},
  {"png", CONV_PNG},
  {"csv", CONV_CSV},
  {"pdf", CONV_PDF},
  {NULL, CONV_NONE},
};

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE OUTFILE\n"
"\n"
"Convert ridge data files to other file types.\n"
"\n"
"  -s              Generate SVG output [default]\n"
"  -d              Generate PDF output\n"
"  -c              Generate CSV file\n"
"  -p              Generate PNG mask\n"
"  -t              Generate TIFF mask\n"
"  -h              Display this message and exit\n"
"\n"
"Loads extracted ridges from INFILE, and generates an alternative\n"
"representation of the ridge data in OUTFILE.\n"
"\n"
"If no data type to generate is specified, attempts to guess from the\n"
"name of OUTFILE.\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);

  exit (status);
}

int
main (int argc, char **argv)
{
  char *filename, *out_filename;
  RioData *data;
  int c;
  int status;
  int conv = CONV_NONE;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 'c':
      conv = CONV_CSV;
      break;
    case 'd':
      conv = CONV_PDF;
      break;
    case 's':
      conv = CONV_SVG;
      break;
    case 'p':
      conv = CONV_PNG;
      break;
    case 't':
      conv = CONV_TIFF;
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

  /* Get input & output filenames */
  if (argc - optind < 2) {
    fprintf (stderr, "ERROR: You must specify input & output filenames.\n\n");
    usage (argv[0], 1);
  }
  filename = argv[optind];
  out_filename = argv[optind+1];

  /* Work out output format from table of filename suffix conversions */
  if (conv == CONV_NONE) {
    const char *out_suffix = strrchr (out_filename, '.');
    if (out_suffix != NULL) {

      out_suffix++; /* Skip '.' */
      for (struct _ConvSuffix *s = conv_suffixes;
           s->suffix != NULL; s++) {
        if (strcasecmp (out_suffix, s->suffix) == 0) {
          conv = s->conv;
          break;
        }
      }
    }
  }

  /* Default to SVG */
  if (conv == CONV_NONE) conv = CONV_SVG;

  /* Attempt to load input file */
  data = rio_data_from_file (filename);
  if (data == NULL) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Could not load ridge data from %s: %s\n",
             filename, msg);
    exit (2);
  }

  /* Generate output */
  switch (conv) {
  case CONV_CSV:
    status = conv_csv (data, out_filename);
    break;
  case CONV_SVG:
    status = conv_svg (data, out_filename);
    break;
  case CONV_PDF:
    status = conv_pdf (data, out_filename);
    break;
  case CONV_TIFF:
    status = conv_tif (data, out_filename);
    break;
  case CONV_PNG:
    status = conv_png (data, out_filename);
    break;
  default:
    abort ();
  }

  rio_data_destroy (data);

  return status ? 0 : 3;
}
