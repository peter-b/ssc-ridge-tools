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

#ifndef __RIDGETOOL_H__
#define __RIDGETOOL_H__

#include "ridgeutil.h"

/* -------------------------------------------------------------------------- */

typedef struct _SurfaceView SurfaceView;

struct _SurfaceView {
  int len;
  int ofs;
  int stride;
  RutSurface *target;
};

/* Creates a new surface view backed by target. The surface view is
 * otherwise uninitialised. */
SurfaceView *surface_view_new (RutSurface *target);

/* Destroys a surface view. */
void surface_view_destroy (SurfaceView *view);


void surface_view_set_row (SurfaceView *view, int row);
void surface_view_set_col (SurfaceView *view, int col);

/* -------------------------------------------------------------------------- */

typedef struct _Filter Filter;

struct _Filter {
  int len;
  int ofs;
  float *data;
};

enum {
  FILTER_FLAG_ROWS = 1 << 0,
  FILTER_FLAG_COLS = 1 << 1,
};

Filter *filter_new (int len);
Filter *filter_new_deriv ();
Filter *filter_new_gaussian (float variance);
void filter_destroy (Filter *f);
void MP_filter (Filter *f, RutSurface *src, RutSurface *dest, int flags);

/* -------------------------------------------------------------------------- */

enum {
  METRICS_MNORM = 1,
  METRICS_NNORM = 2,
  METRICS_ANORM = 3,
};

float metric_Mnorm (float Lpp, float Lqq, float scale);
float metric_Nnorm (float Lpp, float Lqq, float scale);
float metric_Anorm (float Lpp, float Lqq, float scale);

void MP_metrics_SS (RutSurface *src, float scale, int norm,
                    RutSurface *Lp, RutSurface *Lpp, RutSurface *RnormL);

typedef struct _RidgePointsSS RidgePointsSS;
typedef struct _RidgePointsSSEntry RidgePointsSSEntry;

enum {
  EDGE_FLAG_NORTH = 1 << 0,
  EDGE_FLAG_WEST = 1 << 1,
  EDGE_FLAG_SOUTH = 1 << 2,
  EDGE_FLAG_EAST = 1 << 3,
};

struct _RidgePointsSSEntry {
  unsigned char flags;
  unsigned char north;
  unsigned char west;
  unsigned char padding;
};

struct _RidgePointsSS {
  int rows, cols;
  RidgePointsSSEntry *entries;
};

#define RIDGE_POINTS_SS_PTR(s,r,c) ((s)->entries + (s)->cols * (r) + (c))
#define RIDGE_POINTS_SS_REF(s,r,c) (*RIDGE_POINTS_SS_PTR((s),(r),(c)))

RidgePointsSS *ridge_points_SS_new_for_surface (RutSurface *s);
void MP_ridge_points_SS (RidgePointsSS *ridges, RutSurface *Lp, RutSurface *Lpp);
void ridge_points_SS_destroy (RidgePointsSS *r);

/* -------------------------------------------------------------------------- */

typedef struct _RidgeLinesSS RidgeLinesSS;
typedef union _RidgeLinesSSEntry RidgeLinesSSEntry;

union _RidgeLinesSSEntry {
  /* Set member form */
  struct {
      RidgeLinesSSEntry *parent;
      unsigned long long rank;
  } v;
  /* Doubly-linked list member form */
  struct {
    RidgeLinesSSEntry *next;
    RidgeLinesSSEntry *prev;
  } l;
  /* 128-bit integer form */
  __int128_t i;
};

struct _RidgeLinesSS {
  int rows, cols;
  char *raw_entries;
  RidgeLinesSSEntry *entries;
};

#define RIDGE_LINES_SS_PTR(s,r,c) ((s)->entries + (s)->cols * (r) + (c))
#define RIDGE_LINES_SS_REF(s,r,c) (*RIDGE_LINES_SS_PTR((s),(r),(c)))

RidgeLinesSS *ridge_lines_SS_new_for_surface (RutSurface *s);
void MP_ridge_lines_SS_build (RidgeLinesSS *lines, RidgePointsSS *points);
void ridge_lines_SS_destroy (RidgeLinesSS *lines);
void ridge_lines_SS_entry_get_position (RidgeLinesSS *lines,
                                        RidgeLinesSSEntry *entry,
                                        int *row, int *col);
RidgeLinesSSEntry *ridge_lines_SS_entry_next (RidgeLinesSSEntry *entry);
RidgeLinesSSEntry *ridge_lines_SS_entry_prev (RidgeLinesSSEntry *entry);

/* -------------------------------------------------------------------------- */

int export_points (RidgePointsSS *ridges, RutSurface *image,
                   RutSurface *RnormL, const char *filename);
int export_segments (RidgePointsSS *ridges, RutSurface *image,
                     RutSurface *RnormL, const char *filename);
int export_lines (RidgeLinesSS *lines, RidgePointsSS *ridges,
                  RutSurface *image, RutSurface *RnormL, const char *filename);

/* -------------------------------------------------------------------------- */

/* Calculates the eigenvalues and eigenvectors of a symmetric, real
 * 2x2 matrix. The matrix is specified by the A11, A12 and A22
 * arguments, and the l1, l2, and e11, e12, e21, e22 arguments specify
 * the return locations for eigenvalues and eigenvector components:
 *
 *      [ A11  A12 ]     [ e11  e21 ]   [ l1   0 ]   [ e11 e12 ]
 *      [ A12  A22 ]  =  [ e12  e22 ] * [  0  l2 ] * [ e21 e22 ]
 */
void
eigen_symm2x2 (float A11, float A12, float A22, float *l1, float *l2,
               float *e11, float *e12, float *e21, float *e22);

static inline unsigned int
count_bits_set (unsigned int v)
{
  unsigned int c;
  v = v - ((v >> 1) & 0x55555555);                    // reuse input as temporary
  v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
  c = (((v + (v >> 4)) & 0xF0F0F0F) * 0x1010101) >> 24; // count
  return c;
}

/* Linear interpolation: returns value at offset d along x-y. d must
 * be in the range 0.0 - 1.0. */
static inline float
LINTERP (float d, float x, float y)
{
  return (1-d)*x + d*y;
}

#endif /* !__RIDGETOOL_H__ */
