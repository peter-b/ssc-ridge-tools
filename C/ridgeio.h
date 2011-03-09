#ifndef __RIDGEIO_H__
#define __RIDGEIO_H__

#include <stdio.h>
#include <stdint.h>

typedef struct _RioArray RioArray;
typedef struct _RioPoint RioPoint;
typedef struct _RioSegment RioSegment;
typedef RioArray RioLine;

struct _RioArray {
  uint32_t len, capacity;
  size_t size;
  void *contents;
};

struct _RioPoint {
  uint32_t row, col;
  float brightness, strength;
};

struct _RioSegment {
  RioPoint start, end;
};

typedef struct _RioData RioData;

enum {
  RIO_DATA_POINTS = 1,
  RIO_DATA_SEGMENTS = 2,
  RIO_DATA_LINES = 3,
};

struct _RioData {
  uint32_t type;
  RioArray contents;
};

void rio_array_init (RioArray *array, size_t elem_size, uint32_t capacity);
void rio_array_clear (RioArray *array);
void *rio_array_add_i (RioArray *array);
#define rio_array_add(a, T) ((T *) rio_array_add_i (a))
#define rio_array_get_item(a, i, T) ((T *) (a)->contents + (i))
#define rio_array_get_length(a) ((a)->len)

RioData *rio_data_new (int type);
void rio_data_destroy (RioData *data);

#define rio_data_get_type(d) (d->type)
#define rio_data_get_num_entries(d) (rio_array_get_length(&d->contents))
RioPoint *rio_data_get_point (RioData *data, int index);
RioSegment *rio_data_get_segment (RioData *data, int index);
RioLine *rio_data_get_line (RioData *data, int index);
#define rio_data_new_point(d) (rio_array_add(&d->contents, RioPoint))
#define rio_data_new_segment(d) (rio_array_add(&d->contents, RioSegment))
RioLine *rio_data_new_line (RioData *d);

void rio_point_get_position (RioPoint *point, int *row, int *col);
void rio_point_get_subpixel (RioPoint *point, double *row, double *col);
float rio_point_get_brightness (RioPoint *point);
float rio_point_get_strength (RioPoint *point);
void rio_point_set_position (RioPoint *point, int row, int col);
void rio_point_set_subpixel (RioPoint *point, double row, double col);
void rio_point_set_brightness (RioPoint *point, float brightness);
void rio_point_set_strength (RioPoint *point, float strength);

RioPoint *rio_segment_get_start (RioSegment *segment);
RioPoint *rio_segment_get_end (RioSegment *segment);

#define rio_line_init(l,c) (rio_array_init((l), sizeof(RioLine), (c)))
#define rio_line_clear rio_array_clear
#define rio_line_get_length rio_array_get_length
#define rio_line_get_point(l, i) (rio_array_get_item((l), i, RioPoint))
#define rio_line_new_point(l) (rio_array_add ((l), RioPoint))

/* Low-level IO functions */

int rio_write_uint32 (uint32_t val, FILE *fp);
int rio_write_float (float val, FILE *fp);
int rio_read_uint32 (uint32_t *val, FILE *fp);
int rio_read_float (float *val, FILE *fp);

/* Mid-level IO functions */

int rio_point_write (RioPoint *point, FILE *fp);
int rio_segment_write (RioSegment *segment, FILE *fp);
int rio_line_write (RioLine *line, FILE *fp);
int rio_point_read (RioPoint *point, FILE *fp);
int rio_segment_read (RioSegment *segment, FILE *fp);
int rio_line_read (RioLine *line, FILE *fp);

int rio_data_write_header (uint32_t type, uint32_t length, FILE *fp);
int rio_data_read_header (uint32_t *type, uint32_t *length, FILE *fp);

/* High-level IO functions */

int rio_data_to_file (RioData *data, const char *filename);
RioData *rio_data_from_file (const char *filename);

#endif /* !__RIDGEIO_H__ */
