#include "config.h"

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "ridgeio.h"

#define INITIAL_LENGTH 4

int
rio_line_write (RioLine *line, FILE *fp)
{
  assert (line);

  /* Write the number of points in the line */
  if (!rio_write_uint32 (rio_line_get_length (line), fp)) return 0;

  /* Write each of the constituent points */
  for (int i = 0; i < rio_line_get_length (line); i++) {
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
  rio_line_clear (line);

  /* Read the number of points in the line */
  if (!rio_read_uint32 (&len, fp)) return 0;

  /* Allocate the required memory */
  rio_line_init (line, len);

  /* Read in each of the constituent points */
  for (int i = 0; i < len; i++) {
    if (!rio_point_read (rio_line_new_point (line), fp)) return 0;
  }
  return 1;
}
