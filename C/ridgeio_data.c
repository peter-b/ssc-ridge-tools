#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "ridgeio.h"

#define INITIAL_LENGTH 256
#define FILE_MAGIC "RIDG"

typedef struct _RioMetadata RioMetadata;

struct _RioMetadata {
  uint32_t key;
  char *val;
  size_t val_size;
};

RioData *
rio_data_new (int type)
{
  RioData *d = malloc (sizeof (RioData));
  size_t elem_size;

  switch (type) {
  case RIO_DATA_POINTS:
    elem_size = sizeof (RioPoint);
    break;
  case RIO_DATA_SEGMENTS:
    elem_size = sizeof (RioSegment);
    break;
  case RIO_DATA_LINES:
    elem_size = sizeof (RioLine);
    break;
  default:
    abort();
  }

  d->type = type;
  rio_array_init (&d->contents, elem_size, INITIAL_LENGTH);
  rio_array_init (&d->metadata, sizeof (RioMetadata), 0);
  return d;
}

void
rio_data_destroy (RioData *data)
{
  if (!data) return;

  if (rio_array_get_length (&data->contents)
      && data->type == RIO_DATA_LINES) {
    /* If this is a set of line data, it's necessary to free each of the
     * lines' point arrays. */
    for (int i = 0; i < rio_data_get_num_entries (data); i++) {
      RioLine *l = rio_data_get_line (data, i);
      rio_line_clear (l);
    }
  }
  rio_array_clear (&data->contents);

  for (int i = 0; i < rio_array_get_length (&data->metadata); i++) {
    RioMetadata *m = rio_array_get_item (&data->metadata, i, RioMetadata);
    if (m->val) free (m->val);
  }
  rio_array_clear (&data->metadata);

  free (data);
}

/* -------------------------------------------------------------------------- */

RioLine *
rio_data_new_line (RioData *data) {
  RioLine *l = rio_array_add (&data->contents, RioLine);
  rio_line_init (l, 0);
  return l;
}

RioPoint *
rio_data_get_point (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= rio_data_get_num_entries (data)) return NULL;
  if (rio_data_get_type (data) != RIO_DATA_POINTS) return NULL;

  return rio_array_get_item (&data->contents, index, RioPoint);
}

RioSegment *
rio_data_get_segment (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= rio_data_get_num_entries (data)) return NULL;
  if (rio_data_get_type (data) != RIO_DATA_SEGMENTS) return NULL;

  return rio_array_get_item (&data->contents, index, RioSegment);
}

RioLine *
rio_data_get_line (RioData *data, int index)
{
  assert (data);
  if (index < 0 || index >= rio_data_get_num_entries (data)) return NULL;
  if (rio_data_get_type (data) != RIO_DATA_LINES) return NULL;

  return rio_array_get_item (&data->contents, index, RioLine);
}

/* -------------------------------------------------------------------------- */

RioMetadata *find_metadata (RioData *data, uint32_t key, bool create)
{
  RioMetadata *m;
  for (int i = 0; i < rio_array_get_length (&data->metadata); i++) {
    m = rio_array_get_item (&data->metadata, i, RioMetadata);
    if (m->key == key) return m;
  }

  if (!create) return NULL;

  m = rio_array_add (&data->metadata, RioMetadata);
  m->key = key;
  m->val = NULL;
  m->val_size = 0;
  return m;
}

const char *
rio_data_get_metadata (RioData *data, uint32_t key, size_t *val_size)
{
  RioMetadata *m = find_metadata(data, key, false);
  if (!m) return NULL;

  if (val_size && m->val)
    *val_size = m->val_size;
  return m->val;
}

void
rio_data_set_metadata (RioData *data, uint32_t key,
                       const char *value, size_t val_size)
{
  RioMetadata *m = find_metadata(data, key, true);

  if (value) {
    m->val = realloc (m->val, val_size);
    m->val_size = val_size;
    memcpy (m->val, value, val_size);
  } else {
    free (m->val);
    m->val = NULL;
    m->val_size = 0;
  }
}

/* -------------------------------------------------------------------------- */

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

static int
write_metadata_buf (const char *buf, size_t len, FILE *fp)
{
  if (len == -1) len = strlen (buf);
  /* Write out length */
  if (!rio_write_uint32 (len, fp)) return 0;
  /* Write out value. */
  if (fwrite (buf, len, 1, fp) != 1) return 0;
  /* Add padding to bring up to a multiple of 4 bytes. */
  while (len++ % 4) {
    if (fputc (0, fp) == EOF) return 0;
  }
  return 1;
}

int
rio_data_write_metadata (RioData *data, FILE *fp)
{
  /* Save the current location. We'll come back here and fill in the
   * number of metadata items later. */
  int len_pos = ftell (fp);
  if (len_pos < 0) return 0;
  int num_metadata = 0;
  if (!rio_write_uint32 (0, fp)) return 0;

  for (int i = 0; i < rio_array_get_length (&data->metadata); i++) {
    RioMetadata *m = rio_array_get_item (&data->metadata, i, RioMetadata);
    if (m->val == NULL) continue; /* Skip invalid entries */

    if (!rio_write_uint32 (m->key, fp)) return 0;
    if (!write_metadata_buf (m->val, m->val_size, fp)) return 0;

    num_metadata++;
  }

  /* Go back and fill in the number of metadata entries */
  return (fseek (fp, len_pos, SEEK_SET) != -1
          && rio_write_uint32 (num_metadata, fp)
          && fseek (fp, len_pos, SEEK_END) != -1);
}

static char *
read_metadata_buf (FILE *fp, size_t *len)
{
  uint32_t buf_len;
  char *buf;
  /* Read in length */
  if (!rio_read_uint32 (&buf_len, fp)) return NULL;
  /* Allocate space for value. If no return location is specified for
   * length, allow enough space for terminating nul byte. */
  if (len) {
    *len = (size_t) buf_len;
    buf = malloc (buf_len);
  } else {
    buf = malloc (buf_len + 1);
    buf[buf_len] = 0;
  }
  /* Read in value */
  if (fread (buf, buf_len, 1, fp) != 1) return NULL;
  /* Skip over padding to bring up to a multiple of 4 bytes. */
  while (buf_len++ % 4) {
    if (fgetc (fp) == EOF) return NULL;
  }

  return buf;
}

int
rio_data_read_metadata (RioData *data, FILE *fp)
{
  /* Read the number of metadata items */
  uint32_t num_metadata;
  if (!rio_read_uint32 (&num_metadata, fp)) return 0;

  for (int i = 0; i < num_metadata; i++) {
    uint32_t key;
    char *val;
    size_t val_size;

    if (!rio_read_uint32 (&key, fp)) return 0;
    val = read_metadata_buf (fp, &val_size);
    if (val == NULL) return 0;

    rio_data_set_metadata (data, key, val, val_size);
    free (val);
  }
  return 1;
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
  if (!rio_data_write_header (rio_data_get_type (data),
                              rio_data_get_num_entries (data), fp))
    goto save_fail;

  /* Write each entry */
  for (int i = 0; i < rio_data_get_num_entries (data); i++) {
    switch (rio_data_get_type (data)) {
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

  /* Write metadata */
  if (!rio_data_write_metadata (data, fp)) goto save_fail;

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
      if (!rio_point_read (rio_data_new_point (result), fp)) goto load_fail;
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

  /* Read metadata */
  if (!rio_data_read_metadata (result, fp)) goto load_fail;

  fclose (fp);

  return result;

 load_fail:
  errsv = errno;
  rio_data_destroy (result);
  if (fp) fclose (fp);
  errno = errsv;
  return NULL;
}

int
rio_data_get_metadata_uint32 (RioData *data, uint32_t key, uint32_t *val)
{
  const char *buf;
  size_t buf_size;
  uint32_t v = 0;

  buf = rio_data_get_metadata (data, key, &buf_size);
  if (buf_size != 4) {
    fprintf (stderr, "WARNING: Metadata entry %i has length %zi "
             "(expected 4)\n", key, buf_size);
    return 0;
  }

  for (int i = 0; i < 4; i++) {
    v = (v << 8) | buf[i];
  }
  *val = v;
  return 1;
}

void
rio_data_set_metadata_uint32 (RioData *data, uint32_t key, uint32_t val)
{
  unsigned char buf[4];

  for (int i = 0; i < 4; i++) {
    buf[3-i] = val & 0xff;
    val = val >> 8;
  }

  rio_data_set_metadata (data, key, (char *) buf, 4);
}
