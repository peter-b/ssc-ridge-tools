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

#ifndef __RIDGEIO_H__
#define __RIDGEIO_H__

#include <stdio.h>
#include <stdint.h>

/* The Ridge IO library is used to load and save ridge data files.
 * This is a binary file format designed to store ridge data densely,
 * and is optimised for sequential processing.
 *
 * Ridge data files store either ridge points, ridge segments
 * (straight line segments joining two points) or ridge lines
 * (loop-free sequences of two or more points joined by straight line
 * segments).  The files also include a metadata section, which can be
 * in arbitrary formats.
 *
 * In general, you should use the accessor functions to manipulate
 * Ridge IO data structures rather than modifying the fields
 * directly. */

/* ------------------------------------------------------------ */

/* A RioArray is an automatically-expanding array structure. Since
 * such arrays are used in several places by this library, they have
 * been abstracted out. User code should not normally directly modify
 * these arrays -- they should be considered an implementation detail
 * of the library. */

typedef struct _RioArray RioArray;

struct _RioArray {
  uint32_t len, capacity;
  size_t size;
  void *contents;
};

/* Initialise a RioArray. array should be a pointer to a region of
 * memory in which to create the array.  The elem_size is the size of
 * each entry in the array, and the capacity is the initial capacity
 * that should be provided for the array. */
void rio_array_init (RioArray *array, size_t elem_size, uint32_t capacity);

/* Empty a RioArray.  After calling this function, the array will
 * contain no entries and will have zero capacity. */
void rio_array_clear (RioArray *array);

/* Used by rio_array_add() */
void *rio_array_add_i (RioArray *array);

/* Allocate and obtain a pointer to a new entry at the end of an array
 * a for an element of type T.  When you wish to add an element to the
 * array, you should call rio_array_add() to obtain a pointer to the
 * element's storage, and then populate the location with the data to
 * be stored. */
#define rio_array_add(a, T) ((T *) rio_array_add_i (a))

/* Get the entry in an array a of type T at offset i.  Implemented as
 * a macro for speed. */
#define rio_array_get_item(a, i, T) ((T *) (a)->contents + (i))

/* Get the current number of entries an array */
#define rio_array_get_length(a) ((a)->len)

/* Remove the entry in the array at the given index. For speed, this
 * removal is carried out by moving the last entry in the array on top
 * of the entry to be removed, so this operation does not preserve
 * ordering. */
void rio_array_remove (RioArray *array, int index);

/* ------------------------------------------------------------ */

/* A RioPoint represents a single ridge point. It contains the
 * location of the ridge point in 32-bit unsigned fixed point format
 * (with 7 bits in the fractional part) and the brightness and
 * strength of the point in 32-bit IEEE floating-point format. */

typedef struct _RioPoint RioPoint;

struct _RioPoint {
  uint32_t row, col;
  float brightness, strength;
};

/* Get the position of a point in fixed point format. The return data
 * is stored in locations specified, which may be NULL. */
void rio_point_get_position (RioPoint *point, int *row, int *col);

/* Get the position of a point in floating point format. The row and
 * col pointers may be NULL. */
void rio_point_get_subpixel (RioPoint *point, double *row, double *col);

/* Get the brightness of the original image at a ridge point. */
float rio_point_get_brightness (RioPoint *point);

/* Get the ridge strength metric of a ridge point */
float rio_point_get_strength (RioPoint *point);

/* Set the position of a point, in fixed point format (i.e. in 128ths
 * of a pixel). */
void rio_point_set_position (RioPoint *point, int row, int col);

/* Set the position of a pixel in floating point format. The value
 * passed will be rounded to the nearest 128th of a pixel to convert
 * it to fixed point.*/
void rio_point_set_subpixel (RioPoint *point, double row, double col);

/* Set the brightness of the original image for a point. */
void rio_point_set_brightness (RioPoint *point, float brightness);

/* Set the ridge strength metric for a point. */
void rio_point_set_strength (RioPoint *point, float strength);

/* ------------------------------------------------------------ */

/* A RioSegment contains two points, and represents a single straight
 * line usually spanning a single pixel. */

typedef struct _RioSegment RioSegment;

struct _RioSegment {
  RioPoint start, end;
};

RioPoint *rio_segment_get_start (RioSegment *segment);
RioPoint *rio_segment_get_end (RioSegment *segment);

/* ------------------------------------------------------------ */

/* A RioLine contains a sequence of two or more RioPoints,
 * representing a continuous sequence of straight line segments. It's
 * implemented by using a RioArray directly (but since this might
 * change, use the rio_line macros). */

typedef RioArray RioLine;

typedef struct _RioData RioData;

#define rio_line_init(l,c) (rio_array_init((l), sizeof(RioPoint), (c)))
#define rio_line_clear rio_array_clear
#define rio_line_get_length rio_array_get_length
#define rio_line_get_point(l, i) (rio_array_get_item((l), i, RioPoint))
#define rio_line_new_point(l) (rio_array_add ((l), RioPoint))

/* ------------------------------------------------------------ */

/* A RioData structure is the top-level data type for ridge data
 * files. It contains the ridge data - points, segments or lines - and
 * any associated metadata. */

struct _RioData {
  uint32_t type;
  RioArray contents, metadata;
};

/* Constants used to describe Ridge IO data types. */
enum {
  RIO_DATA_POINTS = 1,
  RIO_DATA_SEGMENTS = 2,
  RIO_DATA_LINES = 3,
};

/* Allocate a new RioData structure of the given type. */
RioData *rio_data_new (int type);

/* Destroy a RioData structure, freeing all its allocated memory. */
void rio_data_destroy (RioData *data);

/* Get the type of data stored in a ridge data set. */
#define rio_data_get_type(d) (d->type)

/* Get the number of entries in a ridge data set. */
#define rio_data_get_num_entries(d) (rio_array_get_length(&d->contents))

/* Get the ridge point at the given index from a ridge data set.  If
 * data does not contain ridge points, or the index is out of bounds,
 * returns NULL. */
RioPoint *rio_data_get_point (RioData *data, int index);

/* Get the ridge segment at the given index from a ridge data set.  If
 * data does not contain ridge segments, or the index is out of
 * bounds, returns NULL. */
RioSegment *rio_data_get_segment (RioData *data, int index);

/* Get the ridge line at the given index from a ridge data set.  If
 * data does not contain ridge line, or the index is out of bounds,
 * returns NULL. */
RioLine *rio_data_get_line (RioData *data, int index);

/* Add a new ridge point to a ridge data set and return a pointer to
 * it.  Warning: this macro doesn't check that d actually contains
 * points. */
#define rio_data_new_point(d) (rio_array_add(&d->contents, RioPoint))

/* Add a new ridge segment to a ridge data set and return a pointer to
 * it.  Warning: this macro doesn't check that d actually contains
 * points. */
#define rio_data_new_segment(d) (rio_array_add(&d->contents, RioSegment))

/* Add a new, empty ridge line to a ridge data set and return a
 * pointer to it.  Warning: this function doesn't check that d actually
 * contains lines. */
RioLine *rio_data_new_line (RioData *d);

/* Remove an entry from a ridge data set, freeing any resources
 * associated with that entry.  For speed, this moves the last entry
 * in the data set on top of the entry to be removed; it does not
 * preserve ordering. */
void rio_data_remove_entry (RioData *data, int index);

/* Metadata key constants */
enum {
  /* Num rows in the original image (32-bit unsigned) */
  RIO_KEY_IMAGE_ROWS = 0x10,
  /* Num cols in the original image (32-bit unsigned) */
  RIO_KEY_IMAGE_COLS = 0x11,
  /* Entry classification data (array of 8-bit unsigned) */
  RIO_KEY_IMAGE_CLASSIFICATION = 0x80,
  /* Entry classification likelihoods (array of 32-bit floating point) */
  RIO_KEY_IMAGE_CLASS_LIKELIHOOD = 0x81,
};

/* Get the value associated with a metadata key for data. val_size is
 * the location to store the length of the value (may be NULL) */
const char *rio_data_get_metadata (RioData *data, uint32_t key,
                                   size_t *val_size);

/* Set the metadata value associated with key for data. val_size
 * should be the length of the value.  A copy of the value is made &
 * stored in the metadata. */
void rio_data_set_metadata (RioData *data, uint32_t key,
                            const char *value, size_t val_size);

/* Set the metadata value associated with key for data. val_size
 * should be the length of the value.  This differs from
 * rio_data_set_metadata() in that no copy is made -- after this call
 * the value pointer should be considered owned by the data
 * structure. */
void rio_data_take_metadata (RioData *data, uint32_t key,
                             char *value, size_t val_size);

/* ------------------------------------------------------------ */

/* These IO functions are fairly self-explanatory. They return
 * non-zero on success. */

/* Low-level IO functions */

float rio_htonf (float val);
float rio_ntohf (float val);
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
int rio_data_write_metadata (RioData *data, FILE *fp);
int rio_data_read_metadata (RioData *data, FILE *fp);

/* High-level IO functions */

/* Save ridge data to filename.  Return non-zero on success. */
int rio_data_to_file (RioData *data, const char *filename);
/* Read ridge data from filename.  Return NULL on failure. */
RioData *rio_data_from_file (const char *filename);

/* Convenience functions */

/* Get a metadata value as a 32-bit unsigned integer. */
int rio_data_get_metadata_uint32 (RioData *data, uint32_t key, uint32_t *val);
/* Set a metadata value as a 32-bit unsigned integer. */
void rio_data_set_metadata_uint32 (RioData *data, uint32_t key, uint32_t val);

#endif /* !__RIDGEIO_H__ */
