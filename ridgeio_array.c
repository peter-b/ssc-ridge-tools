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

#include "config.h"

#include <stdlib.h>
#include <assert.h>

#include "ridgeio.h"

void
rio_array_init (RioArray *array, size_t elem_size, uint32_t capacity)
{
  array->len = 0;
  array->capacity = capacity;
  array->size = elem_size;

  array->contents = NULL;
  if (capacity) {
    array->contents = realloc (array->contents, capacity * elem_size);
  }
}

void *
rio_array_add_i (RioArray *array)
{
  if (array->len >= array->capacity) {
    uint32_t new_cap = array->capacity ? (array->capacity << 1) : 1;

    void *ptr = realloc (array->contents, new_cap * array->size);
    assert (ptr);
    array->capacity = new_cap;
    array->contents = ptr;
  }

  return (void *) ((char *) array->contents
                   + (array->size * array->len++));
}

void rio_array_clear (RioArray *array)
{
  array->len = 0;
  array->capacity = 0;
  free (array->contents);
  array->contents = NULL;
}

void
rio_array_remove (RioArray *array, int index) {
  if (index > array->len) return;
  void *tgt = (void *) ((char *) array->contents
                        + (array->size * index));
  void *last = (void *) ((char *) array->contents
                         + (array->size * (array->len - 1)));
  memcpy (tgt, last, array->size);
  array->len--;
}
