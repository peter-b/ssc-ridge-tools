#include "config.h"

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "ridgetool.h"

int main(int argc, char **argv)
{
  float A11, A12, A22;
  float l1, l2, e11, e12, e21, e22;
  float b11, b12, b22;
  int repeats = 1;

  if (argc < 4) {
    printf ("Must specify three values\n");
    exit (1);
  }

  sscanf (argv[1], "%f", &A11);
  sscanf (argv[2], "%f", &A12);
  sscanf (argv[3], "%f", &A22);

  if (argc > 4) {
    sscanf (argv[4], "%i", &repeats);
  }

  printf ("Input: [%f %f; %f %f]\n", A11, A12, A12, A22);

  for (int i = 0; i < repeats; i++)
    eigen_symm2x2 (A11, A12, A22, &l1, &l2, &e11, &e12, &e21, &e22);

  printf ("Eigenvalues: %f and %f\n", l1, l2);
  printf ("Eigenvectors: [%f %f] and [%f %f]\n", e11, e12, e21, e22);

  b11 = e11*e11*l1 + e21*e21*l2;
  b12 = e11*e12*l1 + e21*e22*l2;
  b22 = e12*e12*l1 + e22*e22*l2;

  printf ("Reconstructed: [%f %f; %f %f]\n", b11, b12, b12, b22);

  return 0;
}
