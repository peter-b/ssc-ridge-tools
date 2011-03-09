#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "ridgeio.h"

#define INITIAL_LENGTH 4

int
rio_line_get_length (RioLine *line)
{
  assert (line);
  return line->length;
}

RioPoint *
rio_line_get_point (RioLine *line, int index)
{
  assert (line);
  if (index < 0 || index >= line->length) return NULL;

  return line->points + index;
}

/* Attempt to expand the internal array used by RioLine, if it is
 * full. */
static void
test_expand (RioLine *line)
{
  if (line->length < line->data_length) return;
  int len = line->data_length << 1;
  len = len ? len : INITIAL_LENGTH;

  size_t size = len * sizeof (RioPoint);
  void *newm = realloc (line->points, size);
  assert (newm);
  line->points = newm;
  line->data_length = len;
}

RioPoint *
rio_line_new_point (RioLine *line)
{
  assert (line);
  test_expand (line);
  return line->points + (++line->length);
}

int
rio_line_write (RioLine *line, FILE *fp)
{
  assert (line);

  /* Write the number of points in the line */
  if (!rio_write_uint32 (line->length, fp)) return 0;

  /* Write each of the constituent points */
  for (int i = 0; i < line->length; i++) {
    if (!rio_point_write (rio_line_get_point (line, i), fp)) return 0;
  }
  return 1;
}

int
rio_line_read (RioLine *line, FILE *fp)
{
  assert (line);
  uint32_t len;

  /* First empty the line */
  if (line->points) {
    free (line->points);
  }
  line->points = NULL;
  line->length = 0;
  line->data_length = 0;

  /* Read the number of points in the line */
  if (!rio_read_uint32 (&len, fp)) return 0;

  /* Allocate the required memory */
  line->points = calloc (len, sizeof (RioPoint));
  line->data_length = len;

  /* Read in each of the constituent points */
  for (int i = 0; i < len; i++) {
    if (!rio_point_read (rio_line_new_point (line), fp)) return 0;
  }
  return 1;
}
