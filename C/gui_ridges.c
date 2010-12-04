
#include <math.h>
#include <cairo.h>
#include <gtk/gtk.h>
#include <assert.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"


void
ridge_points_SS_draw_segments (cairo_t *cr, RidgePointsSS *ridges)
{
  for (int row = 0; row < ridges->rows; row++) {
    for (int col = 0; col < ridges->cols; col++) {
      RidgePointsSSEntry *entry = ridges->entries + row * ridges->cols + col;
      double x[2], y[2];
      int n = 0;

      if (count_bits_set (entry->flags) != 2) continue;

      if (entry->flags & EDGE_FLAG_NORTH) {
        x[n] = col + entry->north / 128.0;
        y[n++] = row;
      }

      if (entry->flags & EDGE_FLAG_WEST) {
        y[n] = row + entry->west / 128.0;
        x[n++] = col;
      }

      if (entry->flags & EDGE_FLAG_SOUTH) {
        RidgePointsSSEntry *next =
          ridges->entries + (row + 1) * ridges->cols + col;
        x[n] = col + next->north / 128.0;
        y[n++] = row + 1;
      }

      if (entry->flags & EDGE_FLAG_EAST) {
        RidgePointsSSEntry *next =
          ridges->entries + row * ridges->cols + (col + 1);
        y[n] = row + next->west / 128.0;
        x[n++] = col + 1;
      }

      cairo_move_to (cr, x[0], y[0]);
      cairo_line_to (cr, x[1], y[1]);
      cairo_stroke (cr);
    }
  }
}
