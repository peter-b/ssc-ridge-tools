#include <stdlib.h>

#include "ridgeutil.h"
#include "ridgeconv.h"

#define MIN(x,y) ((x < y) ? x : y)

void
tif_point (RioPoint *p, RutSurface *s)
{
  int row, col;
  rio_point_get_position (p, &row, &col);
  RUT_SURFACE_REF (s, row >> 7, col >> 7) = 1;
}

void
tif_segment (RioPoint *p, RioPoint *q, RutSurface *s)
{
  int row[2], col[2];
  rio_point_get_position (p, &row[0], &col[0]);
  rio_point_get_position (p, &row[1], &col[1]);
  row[0] = MIN(row[0], row[1]);
  col[0] = MIN(col[0], col[1]);
  RUT_SURFACE_REF (s, row[0] >> 7, col[0] >> 7) = 1;
}

int
conv_tif (RioData *data, const char *filename)
{
  int status;

  /* Determine original image height & width */
  uint32_t height, width;
  status = rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_ROWS, &height)
    && rio_data_get_metadata_uint32 (data, RIO_KEY_IMAGE_COLS, &width);
  if (!status) {
    fprintf (stderr, "ERROR: Could not determine image dimensions.\n");
    return 0;
  }

  /* Create output surface. N.b. assume it is initialised to zero. */
  RutSurface *tif = rut_surface_new (height, width);

  /* Populate mask */
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    switch (rio_data_get_type (data)) {
    case RIO_DATA_POINTS:
      tif_point (rio_data_get_point (data, i), tif);
      break;
    case RIO_DATA_SEGMENTS:
      {
        RioSegment *s = rio_data_get_segment (data, i);
        tif_segment (rio_segment_get_start (s), rio_segment_get_end (s), tif);
      }
      break;
    case RIO_DATA_LINES:
      {
        RioLine *l = rio_data_get_line (data, i);
        RioPoint *prev = NULL, *curr;
        for (int j = 0; j < rio_line_get_length (l); j++) {
          curr = rio_line_get_point (l, j);
          if (prev != NULL) {
            tif_segment (prev, curr, tif);
          }
          prev = curr;
        }
      }
      break;
    default:
      abort();
    }
  }

  /* Save mask to disk */
  return rut_surface_to_tiff (tif, filename);
}
