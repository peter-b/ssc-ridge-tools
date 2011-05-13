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

#ifndef __RIDGECONV_H__
#define __RIDGECONV_H__

#include "ridgeio.h"

/* Each of these functions takes a ridge data set (see ridgeio.h) and
 * exports it to a file in the appropriate format with the given
 * filename. */
int conv_csv (RioData *data, const char *filename);
int conv_svg (RioData *data, const char *filename);
int conv_pdf (RioData *data, const char *filename);
int conv_tif (RioData *data, const char *filename);
int conv_png (RioData *data, const char *filename);

#endif /* !__RIDGECONV_H__ */
