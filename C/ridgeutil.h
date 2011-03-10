#ifndef __RIDGEUTIL_H__
#define __RIDGEUTIL_H__

#include <stddef.h>

typedef struct _RutSurface RutSurface;

struct _RutSurface {
  int rows, cols;
  float *data;
};

#define RUT_SURFACE_PTR(s,r,c) ((s)->data + (s)->cols * (r) + (c))
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
void rut_multiproc_free (void *ptr);

#endif /* !__RIDGEUTIL_H__ */
