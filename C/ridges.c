#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ridgetool.h"

struct MPRidgeMaskSSInfo
{
  Surface *Lp, *Lpp, *mask;
};

/* Linear interpolation: returns value at offset d along x-y. d must
 * be in the range 0.0 - 1.0. */
static inline float
LINTERP (float d, float x, float y)
{
  return (1-d)*x + d*y;
}

static float
test_edge_SS (Surface *Lp, Surface *Lpp, int row, int col, int drow, int dcol)
{
  float lp1 = SURFACE_REF (Lp, row, col);
  float lp2 = SURFACE_REF (Lp, row + drow, col + dcol);
  float x, lpp1, lpp2;

  /* Ensure that there is a zero-crossing of Lp on the edge */
  if ((lp1 > 0) ^ (lp2 <= 0)) return -1;

  /* Calculate position along edge */
  x = lp1 / (lp1 - lp2);

  assert (x >= 0);
  assert (x <= 1);

  /* Interpolate value of Lpp */
  lpp1 = SURFACE_REF (Lpp, row, col);
  lpp2 = SURFACE_REF (Lpp, row + drow, col + dcol);
  if (LINTERP (x, lpp1, lpp2) > 0) return -1;
  return x;
}

static void
MP_ridge_mask_SS_func (int thread_num, int thread_count, void *user_data)
{
  struct MPRidgeMaskSSInfo *info = (struct MPRidgeMaskSSInfo *) user_data;
  int first_row, num_rows, row, col;

  first_row = thread_num * (info->Lp->rows / thread_count);
  num_rows = (thread_num + 1) * (info->Lp->rows / thread_count) - first_row;

  for (row = first_row; row < first_row + num_rows; row++) {
    for (col = 0; col < info->Lp->rows; col++) {
      int n_edges = 0;

      /* First handle the case that this square is at the bottom or
       * right of the image. */
      if ((col + 1 >= info->Lp->cols) || (row + 1 >= info->Lp->rows)) {
        SURFACE_REF (info->mask, row, col) = 0;
        continue;
      }

      /* Evil (look, ma, no branches!) */
      n_edges += (test_edge_SS (info->Lp, info->Lpp, row, col, 0, 1) >= 0);
      n_edges += (test_edge_SS (info->Lp, info->Lpp, row, col, 1, 0) >= 0);
      n_edges += (test_edge_SS (info->Lp, info->Lpp, row+1, col, 0, 1) >= 0);
      n_edges += (test_edge_SS (info->Lp, info->Lpp, row, col+1, 1, 0) >= 0);

      SURFACE_REF (info->mask, row, col) = (float) (n_edges == 2);
    }
  }
}

void
MP_ridge_mask_SS (Surface *Lp, Surface *Lpp, Surface **mask)
{
  struct MPRidgeMaskSSInfo *info = malloc (sizeof (struct MPRidgeMaskSSInfo));

  info->Lp = Lp;
  info->Lpp = Lpp;
  info->mask = surface_new_like (Lp);

  MP_task (MP_ridge_mask_SS_func, (void *) info);

  *mask = info->mask;
  free (info);
}
