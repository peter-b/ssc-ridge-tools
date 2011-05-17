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

/* -------------------------------------------------------------------------- */

/* A RutSurface represents a 2D image with 32-bit floating point precision. */

typedef struct _RutSurface RutSurface;

struct _RutSurface {
  int rows, cols;
  float *data;
};

/* Get a pointer to a pixel in a RutSurface at a specified position,
 * where r is the row index and c the column index. */
#define RUT_SURFACE_PTR(s,r,c) ((s)->data + (s)->cols * (r) + (c))
/* Get the value of a pixel in a RutSurface at a specified position,
 * where r is the row index and c the column index. */
#define RUT_SURFACE_REF(s,r,c) (*RUT_SURFACE_PTR((s),(r),(c)))

/* Create a new surface with the given number of rows and
 * columns.  The backing memory is not initialised. */
RutSurface *rut_surface_new (int rows, int cols);

/* Create a new surface with the same dimensions as the surface s.
 * The backing memory is not initialised. */
RutSurface *rut_surface_new_like (RutSurface *s);

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

/* These flags are used to indicate the directions along which a
 * filter should be applied.*/
enum {
  RUT_FILTER_ROWS = 1 << 0,
  RUT_FILTER_COLS = 1 << 1,
};

/* Apply a filter to an image surface.  The flags specify which
 * dimensions to apply the filter along.  Filtering can be carried out
 * in-place by specifying dest=src or dest=NULL. */
void rut_filter_apply_mp (RutFilter *f, RutSurface *src,
                          RutSurface *dest, int flags);

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
