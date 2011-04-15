#include "config.h"

#include <stdlib.h>
#include <math.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <cairo-pdf.h>
#include <gtk/gtk.h>

#include "ridgeconv.h"

#define POINT_RADIUS sqrt (2.0 / M_PI)

static void
svg_single_segment (RioPoint *start, RioPoint *end, cairo_t *cr)
{
  double x[2], y[2];
  rio_point_get_subpixel (start, &y[0], &x[0]);
  rio_point_get_subpixel (end, &y[1], &x[1]);
  cairo_move_to (cr, x[0], y[0]);
  cairo_line_to (cr, x[1], y[1]);
}

static void
svg_points (RioData *data, cairo_t *cr)
{
  double radius = POINT_RADIUS;
  for (int j = 0; j < rio_data_get_num_entries (data); j++) {
    RioPoint *p = rio_data_get_point (data, j);
    double x, y;
    rio_point_get_subpixel (p, &y, &x);
    cairo_arc (cr, x, y, radius, 0, 2 * M_PI);
    cairo_fill (cr);
  }
}

static void
svg_segments (RioData *data, cairo_t *cr)
{
  for (int j = 0; j < rio_data_get_num_entries (data); j++) {
    RioSegment *s = rio_data_get_segment (data, j);
    svg_single_segment (rio_segment_get_start (s),
                        rio_segment_get_end (s),
                        cr);
    cairo_stroke (cr);
  }
}

static void
svg_lines (RioData *data, cairo_t *cr)
{
  for (int j = 0; j < rio_data_get_num_entries (data); j++) {
    RioLine *l = rio_data_get_line (data, j);
    RioPoint *prev = NULL, *curr;
    for (int i = 0; i < rio_line_get_length (l); i++) {
      curr = rio_line_get_point (l, i);
      if (prev != NULL) {
        svg_single_segment (prev, curr, cr);
      }
      prev = curr;
    }
    cairo_stroke (cr);
  }
}

static void
svg_draw (RioData *data, cairo_surface_t *svg)
{
 cairo_t *cr = cairo_create (svg);

  /* Draw background */
  cairo_set_source_rgb (cr, 1, 1, 1);
  cairo_paint (cr);

  cairo_set_line_width (cr, 1);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_source_rgb (cr, 0, 0, 0);

  switch (rio_data_get_type (data)) {
  case RIO_DATA_POINTS:
    svg_points (data, cr);
    break;
  case RIO_DATA_SEGMENTS:
    svg_segments (data, cr);
    break;
  case RIO_DATA_LINES:
    svg_lines (data, cr);
    break;
  default:
    abort ();
  }

  /* Finalise surface */
  cairo_destroy (cr);
}

int
conv_svg (RioData *data, const char *filename)
{
  int status;

  /* Determine original image height & width */
  uint32_t height, width;
  status = rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_ROWS, &height)
    && rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_COLS, &width);
  if (!status) {
    fprintf (stderr, "ERROR: Could not determine image dimensions.\n");
    return 0;
  }

  /* Create output surface */
  cairo_surface_t *svg = cairo_svg_surface_create (filename, width, height);
  if (cairo_surface_status (svg) != CAIRO_STATUS_SUCCESS) {
    fprintf (stderr, "ERROR: Could not create SVG surface for %s: %s\n",
             filename, cairo_status_to_string (cairo_surface_status (svg)));
    return 0;
  }
  /* FIXME naively assumes that all will be fine from now on. */

  svg_draw (data, svg);

  cairo_surface_finish (svg);
  return 1;
 }

int
conv_pdf (RioData *data, const char *filename)
{
  int status;

  /* Determine original image height & width */
  uint32_t height, width;
  status = rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_ROWS, &height)
    && rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_COLS, &width);
  if (!status) {
    fprintf (stderr, "ERROR: Could not determine image dimensions.\n");
    return 0;
  }

  /* Create output surface */
  cairo_surface_t *pdf = cairo_pdf_surface_create (filename, width, height);
  if (cairo_surface_status (pdf) != CAIRO_STATUS_SUCCESS) {
    fprintf (stderr, "ERROR: Could not create PDF surface for %s: %s\n",
             filename, cairo_status_to_string (cairo_surface_status (pdf)));
    return 0;
  }
  /* FIXME naively assumes that all will be fine from now on. */

  svg_draw (data, pdf);

  cairo_surface_finish (pdf);
  return 1;
 }
