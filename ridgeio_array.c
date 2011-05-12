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
