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

/* This header provides definitions for the functions and structures
 * used internally by the 'ridgetool' application. */

#ifndef __RIDGETOOL_H__
#define __RIDGETOOL_H__

#include "ridgeutil.h"

/* -------------------------------------------------------------------------- */

/* Enumeration of different ridge strength metrics. See Lindeberg 1998
 * for details. */
enum {
  METRICS_MNORM = 1,
  METRICS_NNORM = 2,
  METRICS_ANORM = 3,
};

/* Gamma-normalised absolute maximum principal curvature */
float metric_Mnorm (float Lpp, float Lqq, float scale);
/* Square of gamma-normalised square principal curvature difference */
float metric_Nnorm (float Lpp, float Lqq, float scale);
/* Square of gamma-normalised principal curvature difference */
float metric_Anorm (float Lpp, float Lqq, float scale);

/* Calculate single-scale ridge metrics for an image surface at the
 * specified scale, where the ridge strength metric used can be one of
 * METRICS_MNORM, METRICS_NNORM or METRICS_ANORM. Output is to the
 * surfaces Lp, Lpp and RnormL, which should have the same dimensions
 * as the src surface.*/
void MP_metrics_SS (RutSurface *src, float scale, int norm,
                    RutSurface *Lp, RutSurface *Lpp, RutSurface *RnormL);

/* Structure for storing info on the single-scale ridge points in the
 * square formed by four adjacent pixels. The flags field indicates
 * which edges of the square contain ridge points. The north and west
 * fields contain the distance from the top left pixel along the top
 * and left edges of the square at which ridge points are located, in
 * 128ths of pixel spacing (i.e. each of these fields takes a value in
 * the range 0-128 inclusive. If there is no ridge point on the top or
 * left edges, those fields are set to 0xff. */
typedef struct _RidgePointsSSEntry RidgePointsSSEntry;

struct _RidgePointsSSEntry {
  unsigned char flags;
  unsigned char north;
  unsigned char west;
  unsigned char padding;
};

/* Flags for flags field of RidgePointsSSEntry */
enum {
  EDGE_FLAG_NORTH = 1 << 0,
  EDGE_FLAG_WEST = 1 << 1,
  EDGE_FLAG_SOUTH = 1 << 2,
  EDGE_FLAG_EAST = 1 << 3,
};

/* Structure for all single-scale ridge points found in an image */
typedef struct _RidgePointsSS RidgePointsSS;

struct _RidgePointsSS {
  int rows, cols;
  RidgePointsSSEntry *entries;
};

/* Get a pointer to the RidgePointsSSEntry for the square with the
 * pixel (r,c) at the top left. */
#define RIDGE_POINTS_SS_PTR(s,r,c) ((s)->entries + (s)->cols * (r) + (c))
/* Same as RIDGE_POINTS_SS_PTR(), except that the pointer is
 * dereferenced. */
#define RIDGE_POINTS_SS_REF(s,r,c) (*RIDGE_POINTS_SS_PTR((s),(r),(c)))

/* Create a new single-scale ridge point data structure for an image
 * surface. */
RidgePointsSS *ridge_points_SS_new_for_surface (RutSurface *s);
/* Calculate single-scale ridge point locations for the metric images
 * Lp & Lpp. */
void MP_ridge_points_SS (RidgePointsSS *ridges, RutSurface *Lp, RutSurface *Lpp);
/* Destroy single-scale ridge point data, freeing all allocated
 * resources. */
void ridge_points_SS_destroy (RidgePointsSS *r);

/* -------------------------------------------------------------------------- */

/* An entry describing an straight line segment in a single-scale
 * ridge line. One of these is allocated for each square of pixels in
 * an image (i.e. there should be a 1:1 mapping between
 * RidgeLinesSSEntry instances and RidgePointsSSEntry instances).
 *
 * There are two views of an entry: as a member of a disjoint set, and
 * as a member of a doubly-linked list.  Line stitching occurs in two
 * passes.  After all entries are filled with zeros, segments which
 * link up are formed into lines by disjoint set union operations.
 * After all ridge segments have been allocated to a line, the
 * disjoint sets are traversed and transformed in-place into
 * doubly-linked lists. The doubly-linked list form is easier to
 * iterate over for further processing.
 */
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

/* Structure for all ridge line data for an image */

typedef struct _RidgeLinesSS RidgeLinesSS;

struct _RidgeLinesSS {
  int rows, cols;
  char *raw_entries;
  RidgeLinesSSEntry *entries;
};

/* Get a pointer to a ridge line info entry */
#define RIDGE_LINES_SS_PTR(s,r,c) ((s)->entries + (s)->cols * (r) + (c))
/* Get a ridge line info entry directly */
#define RIDGE_LINES_SS_REF(s,r,c) (*RIDGE_LINES_SS_PTR((s),(r),(c)))

/* Allocate a new single-scale ridge line info structure for an image surface */
RidgeLinesSS *ridge_lines_SS_new_for_surface (RutSurface *s);
/* Stitch single-scale ridge lines from a set of ridge points. */
void MP_ridge_lines_SS_build (RidgeLinesSS *lines, RidgePointsSS *points);
/* Free ridge line data */
void ridge_lines_SS_destroy (RidgeLinesSS *lines);
/* Get the coordinates in the image of a particular ridge line entry. */
void ridge_lines_SS_entry_get_position (RidgeLinesSS *lines,
                                        RidgeLinesSSEntry *entry,
                                        int *row, int *col);
/* Traverse a ridge line forwards */
RidgeLinesSSEntry *ridge_lines_SS_entry_next (RidgeLinesSSEntry *entry);
/* Traverse a ridge line backwards */
RidgeLinesSSEntry *ridge_lines_SS_entry_prev (RidgeLinesSSEntry *entry);

/* -------------------------------------------------------------------------- */

/* Functions for exporting single-scale ridge data to ridge data
 * format. This is the best way to convert from the form in which the
 * data is extracted to the more convenient form used by the Ridge IO
 * library. */

/* Export single-scale ridges as ridge points. Returns non-zero on
 * success. */
int export_points (RidgePointsSS *ridges, RutSurface *image,
                   RutSurface *RnormL, const char *filename);
/* Export single-scale ridges as ridge segments. Returns non-zero on
 * success. */
int export_segments (RidgePointsSS *ridges, RutSurface *image,
                     RutSurface *RnormL, const char *filename);

/* Export single-scale ridges as full ridge lines. Returns non-zero on
 * success. */
int export_lines (RidgeLinesSS *lines, RidgePointsSS *ridges,
                  RutSurface *image, RutSurface *RnormL, const char *filename);

/* -------------------------------------------------------------------------- */

/* Calculate the eigenvalues and eigenvectors of a symmetric, real 2x2
 * matrix. The matrix is specified by the A11, A12 and A22 arguments,
 * and the l1, l2, and e11, e12, e21, e22 arguments specify the return
 * locations for eigenvalues and eigenvector components:
 *
 *      [ A11  A12 ]     [ e11  e21 ]   [ l1   0 ]   [ e11 e12 ]
 *      [ A12  A22 ]  =  [ e12  e22 ] * [  0  l2 ] * [ e21 e22 ]
 */
void
eigen_symm2x2 (float A11, float A12, float A22, float *l1, float *l2,
               float *e11, float *e12, float *e21, float *e22);

/* Count the number of bits set in an unsigned integer. */
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
