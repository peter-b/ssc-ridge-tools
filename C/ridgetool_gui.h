cairo_surface_t *surface_to_cairo (Surface *s, float limit);
void show_surface_dialog (Surface *s, float limit);

void ridge_points_SS_draw_segments (cairo_t *cr, RidgePointsSS *ridges);
void ridge_lines_SS_draw_lines (cairo_t *cr, RidgeLinesSS *lines,
                                RidgePointsSS *points);
