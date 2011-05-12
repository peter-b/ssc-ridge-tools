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

#ifndef __RIDGETOOL_GUI_H__
#define __RIDGETOOL_GUI_H__

#include "ridgeutil.h"

cairo_surface_t *surface_to_cairo (RutSurface *s, float limit);
void show_surface_dialog (RutSurface *s, float limit);

void ridge_points_SS_draw_segments (cairo_t *cr, RidgePointsSS *ridges);
void ridge_lines_SS_draw_lines (cairo_t *cr, RidgeLinesSS *lines,
                                RidgePointsSS *points);

#endif /* !__RIDGETOOL_GUI_H__ */
