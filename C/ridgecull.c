#include "config.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include "ridgeio.h"

#define GETOPT_OPTIONS "b:l:s:ih"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... INFILE [OUTFILE]\n"
"\n"
"Cull ridge data files to remove ridges below a threshold.\n"
"\n"
"  -b BRIGHTNESS   Specify a minimum ridge brightness\n"
"  -l LENGTH       Specify a minimum ridge length\n"
"  -s STRENGTH     Specify a minimum ridge strength\n"
"  -i              Invert (cull ridges above thresholds)\n"
"  -h              Display this message and exit\n"
"\n"
"If OUTFILE is not specified, culled data is written back into INFILE.\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);
  exit (status);
}

static void
parse_float_arg (char *name, int optopt, char *optarg, float *dest) {
  int status = sscanf (optarg, "%f", dest);
  if (status == 1) return;
  fprintf (stderr, "ERROR: Invalid argument '%s' for -%c option\n\n.",
           optarg, optopt);
  usage (name, 1);
}

int
main (int argc, char **argv)
{
  RioData *data;
  char *filename, *out_filename = NULL;
  int c, status;
  float min_brightness = -INFINITY, min_length = -INFINITY,
    min_strength = -INFINITY;
  int invert = 0;
  int total_entries = 0, culled_entries = 0;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 'b':
      parse_float_arg (argv[0], optopt, optarg, &min_brightness);
      break;
    case 'l':
      parse_float_arg (argv[0], optopt, optarg, &min_length);
      break;
    case 's':
      parse_float_arg (argv[0], optopt, optarg, &min_strength);
      break;
    case 'i':
      invert = !invert;
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
  } else {
    out_filename = filename;
  }

  /* Attempt to load input file */
  data = rio_data_from_file (filename);
  if (data == NULL) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Could not load ridge data from %s: %s\n",
             filename, msg);
    exit (2);
  }

  /* Actual culling */
  total_entries = rio_data_get_num_entries (data);
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    int cull = 0;
    float brightness = 0, length = 0, strength = 0;

    /* Calculate parameters. For segments and lines, the mean brightness and strength are used. */
    switch (rio_data_get_type (data)) {
    case RIO_DATA_POINTS:
      {
        RioPoint *p = rio_data_get_point (data, i);
        length = 0;
        brightness = rio_point_get_brightness (p);
        strength = rio_point_get_strength (p);
        break;
      }
    case RIO_DATA_SEGMENTS:
      {
        RioSegment *s = rio_data_get_segment (data, i);
        RioPoint *a = rio_segment_get_start (s);
        RioPoint *b = rio_segment_get_end (s);
        length = 1;
        brightness = (rio_point_get_brightness (a)
                      + rio_point_get_brightness (b)) / 2;
        strength = (rio_point_get_strength (a)
                    + rio_point_get_strength (b)) / 2;
        break;
      }
    case RIO_DATA_LINES:
      {
        RioLine *l = rio_data_get_line (data, i);
        int len = rio_line_get_length (l);
        for (int j = 0; j < len; j++) {
          RioPoint *p = rio_line_get_point (l, j);
          brightness += rio_point_get_brightness (p);
          strength += rio_point_get_strength (p);
        }
        length = len - 1;
        brightness /= len;
        strength /= len;
        break;
      }
    default:
      abort ();
    }

    cull |= (invert ^ (min_length > length));
    cull |= (invert ^ (min_brightness > brightness));
    cull |= (invert ^ (min_strength > strength));

    if (cull) {
      rio_data_remove_entry (data, i--);
      culled_entries++;
    }
  }

  /* Attempt to save output file */
  status = rio_data_to_file (data, filename);
  if (!status) {
    const char *msg = errno ? strerror (errno) : "Unexpected error";
    fprintf (stderr, "ERROR: Could not write ridge data to %s: %s\n",
             filename, msg);
    exit (3);
  }

  fprintf (stderr, "Culled %i of %i ridge entries.\n",
           culled_entries, total_entries);

  return 0;
}
