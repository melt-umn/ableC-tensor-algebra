#include <stdio.h>
#include "tensors.xh"

tensor format fy ({dense});
tensor format fA ({dense, sparse}, {1, 0});
tensor format fx ({dense});

int main() {
  tensor<fy> y = build(tensor<fy>)({46});
  tensor<fA> A = build(tensor<fA>)({36, 48});
  tensor<fx> x = build(tensor<fx>)({48});

  A[0,0] = 4.0;
  A[0,3] = 2.0;
  A[7,7] = 6.0;
  A[7,0] = 4.0;

  x[0] = 3.0;
  x[3] = 2.0;
  x[7] = 1.0;

  y[i] = A[i,j] * x[j];

  printf("y[0] = %f (should be %f)\n", y[0], 16.0);
  printf("y[3] = %f (should be %f)\n", y[3], 0.0);
  printf("y[7] = %f (should be %f)\n", y[7], 18.0);
}
