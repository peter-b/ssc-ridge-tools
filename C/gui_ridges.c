
#include <math.h>
#include <cairo.h>
#include <gtk/gtk.h>
#include <assert.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"

static void
svg_path_ridge_segment (cairo_t *cr, RidgePointsSS *ridges,
                        int row, int col)
{
  RidgePointsSSEntry *entry = RIDGE_POINTS_SS_PTR (ridges, row, col);
  double x[2], y[2];
  int n = 0;

  if (count_bits_set (entry->flags) != 2) return;

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
      RIDGE_POINTS_SS_PTR (ridges, row + 1, col);
    x[n] = col + next->north / 128.0;
    y[n++] = row + 1;
  }

  if (entry->flags & EDGE_FLAG_EAST) {
    RidgePointsSSEntry *next =
      RIDGE_POINTS_SS_PTR (ridges, row, col + 1);
    y[n] = row + next->west / 128.0;
    x[n++] = col + 1;
  }

  cairo_move_to (cr, x[0], y[0]);
  cairo_line_to (cr, x[1], y[1]);
}

void
ridge_points_SS_draw_segments (cairo_t *cr, RidgePointsSS *ridges)
{
  for (int row = 0; row < ridges->rows; row++) {
    for (int col = 0; col < ridges->cols; col++) {
      svg_path_ridge_segment (cr, ridges, row, col);
      cairo_stroke (cr);
    }
  }
}

void
ridge_lines_SS_draw_lines (cairo_t *cr, RidgeLinesSS *lines,
                           RidgePointsSS *points)
{
  RidgeLinesSSEntry *iter;
  assert (cr);
  assert (lines);

  for (int i = 0; i < lines->rows; i++) {
    for (int j = 0; j < lines->cols; j++) {
      RidgeLinesSSEntry *line = RIDGE_LINES_SS_PTR (lines, i, j);
      RidgePointsSSEntry *point = RIDGE_POINTS_SS_PTR (points, i, j);

      /* Skip points not at start of line list */
      if (line->l.prev) continue;

      /* Skip points which aren't ridge segments */
      if (count_bits_set (point->flags) != 2) continue;

      /* Walk line, building path */
      for (iter = line; iter != NULL; iter = iter->l.next) {
        int row, col;
        ridge_lines_SS_entry_get_position (lines, iter, &row, &col);
        svg_path_ridge_segment (cr, points, row, col);
      }
      cairo_stroke (cr);
    }
  }
}
