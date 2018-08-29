#include <stdio.h>
#include "tensors.xh"

tensor format fy ({dense});
tensor format fA ({dense, sparse}, {1, 0});
tensor format fx ({dense});

indexvar i, j;

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

int main() {
  tensor<fy> y = build(tensor<fy>)({36});
  tensor<fA> A = build(tensor<fA>)({36, 48});
  tensor<fx> x = build(tensor<fx>)({48});

  A[0,0] = 4.0;
  A[0,3] = 2.0;
  A[7,7] = 6.0;
  A[7,0] = 4.0;

  assert(A[0,0], 4.0);
  assert(A[0,3], 2.0);
  assert(A[7,7], 6.0);
  assert(A[7,0], 4.0);

  x[0] = 3.0;
  x[3] = 2.0;
  x[7] = 1.0;

  y[i] = A[i,j] * x[j];

  assert(y[0], 16.0);
  assert(y[3], 0.0);
  assert(y[7], 18.0);

  freeTensor(y);
  freeTensor(A);
  freeTensor(x);

  if(error) exit(1);
  return 0;
}
