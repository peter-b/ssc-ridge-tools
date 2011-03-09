#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "ridgeio.h"

#define INITIAL_LENGTH 256
#define FILE_MAGIC "RIDG"

RioData *
rio_data_new (int type)
{
  RioData *d = malloc (sizeof (RioData));

  switch (type) {
  case RIO_DATA_POINTS:
  case RIO_DATA_SEGMENTS:
  case RIO_DATA_LINES:
    break;
  default:
    abort();
  }

  d->type = type;
  d->length = 0;
  d->data_length = 0;
  d->entries = NULL;
  return d;
}

void
rio_data_destroy (RioData *data)
{
  if (!data) return;

  if (data->entries) {
    /* If this is a set of line data, it's necessary to free each of the
     * lines' point arrays. */
    if (data->type == RIO_DATA_LINES) {
      for (int i = 0; i < data->length; i++) {
        RioLine *l = rio_data_get_line (data, i);
        free (l->points);
      }
    }
    free (data->entries);
  }
  free (data);
}

int
rio_data_get_type (RioData *data)
{
  assert (data);
  return data->type;
}

int
rio_data_get_num_entries (RioData *data)
{
  assert (data);
  return data->length;
}

RioPoint *
rio_data_get_point (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= data->length) return NULL;
  if (data->type != RIO_DATA_POINTS) return NULL;

  return ((RioPoint *) (data->entries)) + index;
}

RioSegment *
rio_data_get_segment (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= data->length) return NULL;
  if (data->type != RIO_DATA_SEGMENTS) return NULL;

  return ((RioSegment *) (data->entries)) + index;
}

RioLine *
rio_data_get_line (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= data->length) return NULL;
  if (data->type != RIO_DATA_LINES) return NULL;

  return ((RioLine *) (data->entries)) + index;
}

/* Attempt to expand the internal array used by RioData, if it is
 * full. */
static void
test_expand (RioData *data, size_t entry_size)
{
  if (data->length < data->data_length) return;
  int len = data->data_length << 1;
  len = len ? len : INITIAL_LENGTH;

  size_t size = len * entry_size;
  void *newm = realloc (data->entries, size);
  assert (newm);
  data->entries = newm;
  data->data_length = len;
}

RioPoint *
rio_data_new_point (RioData *data)
{
  assert (data);
  if (data->type != RIO_DATA_POINTS) return NULL;
  test_expand (data, sizeof (RioPoint));
  return ((RioPoint *) (data->entries)) + (++data->length);
}

RioSegment *
rio_data_new_segment (RioData *data)
{
  assert (data);
  if (data->type != RIO_DATA_SEGMENTS) return NULL;
  test_expand (data, sizeof (RioSegment));
  return ((RioSegment *) (data->entries)) + (++data->length);
}

RioLine *
rio_data_new_line (RioData *data)
{
  assert (data);
  if (data->type != RIO_DATA_LINES) return NULL;
  test_expand (data, sizeof (RioLine));
  return ((RioLine *) (data->entries)) + (++data->length);
}

int
rio_data_write_header (uint32_t type, uint32_t length, FILE *fp)
{
  return ((fputs (FILE_MAGIC, fp) != EOF)
          && rio_write_uint32 (type, fp)
          && rio_write_uint32 (0, fp) /* Space for future use */
          && rio_write_uint32 (length, fp));
}

int
rio_data_read_header (uint32_t *type, uint32_t *length, FILE *fp)
{
  /* Read file magic */
  char magic_arr[5];
  char *magic = fgets (magic_arr, 5, fp);
  if (!magic || strlen (magic) != 4) {
    return 0;
  }

  /* Check that magic is valid */
  if (strcmp (magic, FILE_MAGIC)) {
    errno = EILSEQ;
    return 0;
  }

  /* Read & verify the file type */
  if (!rio_read_uint32 (type, fp)) return 0;
  switch (*type) {
  case RIO_DATA_POINTS:
  case RIO_DATA_SEGMENTS:
  case RIO_DATA_LINES:
    break;
  default:
    errno = EILSEQ;
    return 0;
  }

  /* Read the number of entries in the file */
  uint32_t dummy;
  return (rio_read_uint32 (&dummy, fp)  /* Space for future use */
          && rio_read_uint32 (length, fp));
}

int
rio_data_to_file (RioData *data, const char *filename) {
  int errsv;
  assert (data);
  assert (filename);

  /* Reset errno */
  errno = 0;

  /* Open the file */
  FILE *fp = fopen (filename, "wb");
  if (!fp) goto save_fail;

  /* Write header */
  if (!rio_data_write_header (data->type, data->length, fp)) goto save_fail;

  /* Write each entry */
  for (int i = 0; i < data->length; i++) {
    switch (data->type) {
    case RIO_DATA_POINTS:
      if (!rio_point_write (rio_data_get_point (data, i), fp)) goto save_fail;
      break;
    case RIO_DATA_SEGMENTS:
      if (!rio_segment_write (rio_data_get_segment (data, i), fp)) goto save_fail;
      break;
    case RIO_DATA_LINES:
      if (!rio_line_write (rio_data_get_line (data, i), fp)) goto save_fail;
      break;
    default:
      abort ();
    }
  }

  fclose (fp);

  return 1;

 save_fail:
  errsv = errno;
  if (fp) fclose (fp);
  errno = errsv;
  return 0;
}

RioData *
rio_data_from_file (const char *filename)
{
  int errsv;
  assert (filename);
  RioData *result = NULL;

  /* Reset errno */
  errno = 0;

  /* Open the file */
  FILE *fp = fopen (filename, "rb");
  if (!fp) goto load_fail;

  /* Read & validate header, and create RioData of appropriate type */
  uint32_t len, type;
  if (!rio_data_read_header (&type, &len, fp)) goto load_fail;
  result = rio_data_new (type);

  /* Read each entry */
  for (int i = 0; i < len; i++) {
    switch (type) {
    case RIO_DATA_POINTS:
      if (rio_point_read (rio_data_new_point (result), fp)) goto load_fail;
      break;
    case RIO_DATA_SEGMENTS:
      if (!rio_segment_read (rio_data_new_segment (result), fp)) goto load_fail;
      break;
    case RIO_DATA_LINES:
      if (!rio_line_read (rio_data_new_line (result), fp)) goto load_fail;
      break;
    default:
      abort ();
    }
  }

  fclose (fp);

  return result;

 load_fail:
  errsv = errno;
  rio_data_destroy (result);
  if (fp) fclose (fp);
  errno = errsv;
  return NULL;
}
