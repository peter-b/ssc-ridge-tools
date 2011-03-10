#include "config.h"

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "ridgeio.h"

int
rio_write_uint32 (uint32_t val, FILE *fp)
{
  for (int i = 0; i < 4; i++) {
    int s = fputc (val >> ((3-i)*8), fp);
    if (s == EOF) return 0;
  }
  return 1;
}

int
rio_write_float (float val, FILE *fp) {
  union {
    uint32_t i;
    float f;
  } u;
  u.f = val;
  return rio_write_uint32 (u.i, fp);
}

int
rio_read_uint32 (uint32_t *val, FILE *fp)
{
  uint32_t v = 0;
  for (int i = 0; i < 4; i ++) {
    int b = fgetc (fp);
    if (b == EOF) return 0;
    v = (v << 8) + b;
  }
  *val = v;
  return 1;
}

int
rio_read_float (float *val, FILE *fp)
{
  union {
    uint32_t i;
    float f;
  } u;
  if (!rio_read_uint32 (&u.i, fp)) return 0;
  *val = u.f;
  return 1;
}
