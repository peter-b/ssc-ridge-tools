#ifndef __RIDGETOOL_H__
#define __RIDGETOOL_H__

/* -------------------------------------------------------------------------- */

typedef struct _Surface Surface;

struct _Surface {
  int rows, cols;
  float *data;
};

#define SURFACE_PTR(s,r,c) ((s)->data + (s)->cols * (r) + (c))
#define SURFACE_REF(s,r,c) (*SURFACE_PTR((s),(r),(c)))

/* Create a new surface with the given number of rows and
 * columns.  The backing memory is not initialised. */
Surface *surface_new (int rows, int cols);

/* Create a new surface with the same dimensions as the surface s.
 * The backing memory is not initialised. */
Surface *surface_new_like (Surface *s);

/* Create a new surface by reading data from the 32-bit floating
 * point, single channel TIFF file specified by filename.  If an error
 * occurs, returns NULL. */
Surface *surface_from_tiff (const char *filename);

/* Save a surface to a 32-bit floating point, single channel TIFF file
 * specified by filename. Returns non-zero on success. */
int surface_to_tiff (Surface *s, const char *filename);

/* Destroys a surface, freeing the underlying memory */
void surface_destroy (Surface *s);

/* -------------------------------------------------------------------------- */

typedef struct _SurfaceView SurfaceView;

struct _SurfaceView {
  int len;
  int ofs;
  int stride;
  Surface *target;
};

/* Creates a new surface view backed by target. The surface view is
 * otherwise uninitialised. */
SurfaceView *surface_view_new (Surface *target);

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
void MP_filter (Filter *f, Surface *src, Surface *dest, int flags);

/* -------------------------------------------------------------------------- */

enum {
  METRICS_MNORM = 1,
  METRICS_NNORM = 2,
  METRICS_ANORM = 3,
};

float metric_Mnorm (float Lpp, float Lqq, float scale);
float metric_Nnorm (float Lpp, float Lqq, float scale);
float metric_Anorm (float Lpp, float Lqq, float scale);

void MP_metrics_SS (Surface *src, float scale, int norm,
                    Surface *Lp, Surface *Lpp, Surface *RnormL);

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

RidgePointsSS *ridge_points_SS_new_for_surface (Surface *s);
void MP_ridge_points_SS (RidgePointsSS *ridges, Surface *Lp, Surface *Lpp);
void ridge_points_SS_destroy (RidgePointsSS *r);

void ridge_points_SS_to_segments_mask (RidgePointsSS *ridges, Surface *mask);
void ridge_points_SS_to_points_mask (RidgePointsSS *ridges, Surface *mask);

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

RidgeLinesSS *ridge_lines_SS_new_for_surface (Surface *s);
void MP_ridge_lines_SS_build (RidgeLinesSS *lines, RidgePointsSS *points);
void ridge_lines_SS_destroy (RidgeLinesSS *lines);
void ridge_lines_SS_entry_get_position (RidgeLinesSS *lines,
                                        RidgeLinesSSEntry *entry,
                                        int *row, int *col);

/* -------------------------------------------------------------------------- */

/* Global variable controlling how many parallel processes to use */
extern int multiproc_threads;

/* Multiprocessing worker function.  This is called once in each
 * worker process or thread, where threadnum is the thread or process
 * index (starting from 0) and threadcount is the total number of
 * threads or processes running the function.  The same user_data is
 * passed to all invocations of the function.  The function should
 * always free any allocated resources, and should return normally on
 * success. */
typedef void (*MultiProcFunc)(int threadnum, int threadcount, void *user_data);

/* Runs func in up to multiproc_threads separate processes or threads,
 * passing it user_data.  If multiproc_threads is less than 2, does
 * runs func in the current thread. */
int MP_task (MultiProcFunc func, void *user_data);

void *MP_malloc (size_t size);
void MP_free (void *ptr);

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

#endif /* !__RIDGETOOL_H__ */