#ifndef __RIDGETOOL_GUI_H__
#define __RIDGETOOL_GUI_H__

#include "ridgeutil.h"

cairo_surface_t *surface_to_cairo (RutSurface *s, float limit);
void show_surface_dialog (RutSurface *s, float limit);

void ridge_points_SS_draw_segments (cairo_t *cr, RidgePointsSS *ridges);
void ridge_lines_SS_draw_lines (cairo_t *cr, RidgeLinesSS *lines,
                                RidgePointsSS *points);

#endif /* !__RIDGETOOL_GUI_H__ */
