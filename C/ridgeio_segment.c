#include "config.h"

#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "ridgeio.h"

RioPoint *
rio_segment_get_start (RioSegment *segment)
{
  assert (segment);
  return &segment->start;
}

RioPoint *
rio_segment_get_end (RioSegment *segment)
{
  assert (segment);
  return &segment->end;
}

int
rio_segment_write (RioSegment *segment, FILE *fp)
{
  return (rio_point_write (rio_segment_get_start (segment), fp)
          && rio_point_write (rio_segment_get_end (segment), fp));
}

int
rio_segment_read (RioSegment *segment, FILE *fp)
{
  return (rio_point_read (rio_segment_get_start (segment), fp)
          && rio_point_read (rio_segment_get_end (segment), fp));
}
