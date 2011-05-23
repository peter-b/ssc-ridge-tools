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

/* The Ridge utils library abstracts out some commonly-used
 * functionality; specifically, 2D image loading and saving from TIFF,
 * and simple multiprocessing capabilities. */

#ifndef __RIDGEUTIL_H__
#define __RIDGEUTIL_H__

#include <stddef.h>

#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

typedef struct _RutExtents RutExtents;

struct _RutExtents {
  int top; /* First row */
  int left; /* First column */
  int scale; /* Smallest scale */
  int height; /* Number of rows */
  int width; /* Number of columns */
  int n_scales; /* Number of scales */
};

/* -------------------------------------------------------------------------- */

/* A RutSurface represents a 2D image with 32-bit floating point precision. */

typedef struct _RutSurface RutSurface;

struct _RutSurface {
  int rows, cols;
  int rowstep, colstep;
  int is_view;
  float *data;
};

#ifndef DEBUG_SURFACE
/* Get a pointer to a pixel in a RutSurface at a specified position,
 * where r is the row index and c the column index. */
  #define RUT_SURFACE_PTR(s,r,c) \
((s)->data + (s)->rowstep * (r) + (s)->colstep * (c))

#else
  #include <assert.h>
static inline float *
RUT_SURFACE_PTR (RutSurface *s, int r, int c)
{
  assert (s);
  assert (r >= 0 && r < s->rows);
  assert (c >= 0 && c < s->cols);
  return s->data + s->rowstep * r + s->colstep * c;
}
#endif /* !DEBUG_SURFACE */

/* Get the value of a pixel in a RutSurface at a specified position,
 * where r is the row index and c the column index. */
#define RUT_SURFACE_REF(s,r,c) (*RUT_SURFACE_PTR((s),(r),(c)))

enum {
  RUT_SURFACE_ROWS = 1 << 0,
  RUT_SURFACE_COLS = 1 << 1,
};

/* Create a new surface with the given number of rows and
 * columns.  The backing memory is not initialised. */
RutSurface *rut_surface_new (int rows, int cols);

/* Create a new surface with the same dimensions as the surface s.
 * The backing memory is not initialised. */
RutSurface *rut_surface_new_like (RutSurface *s);

/* Create a new surface as a view of the surface s. */
RutSurface *rut_surface_new_view (RutSurface *s);

/* Create a new surface as a view of the surface s. */
RutSurface *rut_surface_new_view_extents (RutSurface *s, RutExtents *extents);

/* Swap rows and columns of a surface. */
void rut_surface_transpose (RutSurface *s);

/* Create a new surface by reading data from the 32-bit floating
 * point, single channel TIFF file specified by filename.  If an error
 * occurs, returns NULL. */
RutSurface *rut_surface_from_tiff (const char *filename);

/* Save a surface to a 32-bit floating point, single channel TIFF file
 * specified by filename. Returns non-zero on success. */
int rut_surface_to_tiff (RutSurface *s, const char *filename);

/* Destroys a surface, freeing the underlying memory */
void rut_surface_destroy (RutSurface *s);

/* -------------------------------------------------------------------------- */

/* A RutScaleSpace represents a 3D scale space as a set of image planes. */

typedef struct _RutScaleSpace RutScaleSpace;

struct _RutScaleSpace {
  int rows;
  int cols;
  int n_scales;
  int rowstep, colstep, scalestep;
  int is_view;
  float *scales;
  float *data;
};

#define RUT_SCALE_SPACE_PTR(ss,r,c,s) \
  ((ss)->data + (ss)->rowstep * (r) + (ss)->colstep * (c) + (ss)->scalestep * (s))

#define RUT_SCALE_SPACE_REF(ss,r,c,s) (*RUT_SCALE_SPACE_PTR(ss,r,c,s))

enum {
  RUT_SCALE_SPACE_ROWS = 1,
  RUT_SCALE_SPACE_COLS = 2,
  RUT_SCALE_SPACE_SCALE = 3,
};

/* Create a scale space from an image surface. The scales are sorted
 * and any duplicate values removed, so it is not valid to assume that
 * the ordering and number of scales in the generated scale space will
 * be the same as passed in.  Additionally, if libridgeutil was
 * compiled without the PRECISE_GAUSSIAN flag, scales will be rounded
 * to the nearest 1/3. */
RutScaleSpace *rut_scale_space_generate_mp (RutSurface *image,
                                            int n_scales,
                                            const float *scales);

/* Destroy a scale space, freeing any allocated resources. */
void rut_scale_space_destroy (RutScaleSpace *s);

/* Create an empty scale space with the same dimensions and scales as
 * another scale space. */
RutScaleSpace *rut_scale_space_new_like (RutScaleSpace *s);

/* Create a scale space view. */
RutScaleSpace *rut_scale_space_new_view (RutScaleSpace *s);

/* Create a scale space view limited to a particular subspace of the
 * original scale space. */
RutScaleSpace *rut_scale_space_new_view_extents (RutScaleSpace *s,
                                                 RutExtents *extents);

/* Obtain a planar cross-section of the scale space as an image
 * surface view (i.e. changes to the surface are applied to the scale
 * space). The axis is the axis normal to the plane, and the offset is
 * the position of the plane along that axis. For example, to get the
 * image at a particular scale N, you would call
 * rut_scale_space_get_surface (scale_space, RUT_SCALE_SPACE_SCALE,
 * N). */
RutSurface *rut_scale_space_get_surface (RutScaleSpace *s, int axis, int offset);

/* -------------------------------------------------------------------------- */

/* A Filter represents an arbitrary single-dimension finite impulse
 * response filter, based on an array of values. The ofs field sets
 * the n=0 point in the filter. */

typedef struct _RutFilter RutFilter;

struct _RutFilter {
  int len;
  int ofs;
  float *data;
};

/* Create a new filter of length len. */
RutFilter *rut_filter_new (int len);
/* Create a new derivative filter */
RutFilter *rut_filter_new_deriv ();
/* Create a new Gaussian filter with a given variance */
RutFilter *rut_filter_new_gaussian (float variance);
/* Destroy a filter, releasing its resources */
void rut_filter_destroy (RutFilter *f);

/* Apply a filter to an image surface.  The flags specify which
 * dimensions to apply the filter along.  Filtering can be carried out
 * in-place by specifying dest=src or dest=NULL. */
void rut_filter_surface_mp (RutFilter *f, RutSurface *src,
                            RutSurface *dest, int flags);

/* -------------------------------------------------------------------------- */

/* Tiff utility functions */

/* Obtain the dimensions of a TIFF file without loading it, returning
 * true on success. */
int rut_tiff_get_size (const char *filename, int *rows, int *cols);

/* -------------------------------------------------------------------------- */

/* Global variable controlling how many parallel processes to use */
extern int rut_multiproc_threads;

/* Multiprocessing worker function.  This is called once in each
 * worker process or thread, where threadnum is the thread or process
 * index (starting from 0) and threadcount is the total number of
 * threads or processes running the function.  The same user_data is
 * passed to all invocations of the function.  The function should
 * always free any allocated resources, and should return normally on
 * success. */
typedef void (*RutMultiProcFunc)(int threadnum, int threadcount,
                                 void *user_data);

/* Runs func in up to rut_multiproc_threads separate processes or
 * threads, passing it user_data.  If rut_multiproc_threads is less
 * than 2, runs func in the current thread. */
int rut_multiproc_task (RutMultiProcFunc func, void *user_data);

/* Data areas that should be writable by child tasks for return to the
 * controlling thread must be allocated via the following two
 * functions. */
void *rut_multiproc_malloc (size_t size);
/* Does nothing if ptr is NULL. */
void rut_multiproc_free (void *ptr);

#endif /* !__RIDGEUTIL_H__ */
