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

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <gtk/gtk.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"

#define GETOPT_OPTIONS "l:h"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s OPTION... FILENAME\n"
"\n"
"Display 32-bit floating-point greyscale TIFF files.\n"
"\n"
"  -l LIMIT        Maximum brightness limit\n"
"  -h              Display this message and exit\n"
"\n"
"Please report bugs to p.brett@surrey.ac.uk\n",
name);

  exit (status);
}

int
main (int argc, char **argv)
{
  int c, status;
  float limit = NAN;
  char *filename;
  RutSurface *s;

  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 'l':
      status = sscanf (optarg, "%f", &limit);
      if (status != 1) {
        fprintf (stderr, "ERROR: Bad argument '%s' to -l option.\n\n",
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
  if (argc - optind < 1) {
    fprintf (stderr, "ERROR: You must specify an input filename.\n\n");
    usage (argv[0], 1);
  }
  filename = argv[optind];

  /* Load and display file */
  s = rut_surface_from_tiff (filename);
  if (!s) exit (1);

  gtk_init (&argc, &argv);
  if (isnan (limit)) {
    limit = -INFINITY;
    for (int row = 0; row < s->rows; row++) {
      for (int col = 0; col < s->cols; col++) {
        float v = RUT_SURFACE_REF (s, row, col);
        limit = (v > limit) ? v : limit;
      }
    }
  }
  show_surface_dialog (s, limit);

  rut_surface_destroy (s);
  return 0;
}
