#include "tensors.xh"
#include <stdio.h>

tensor format s1d0 ({sparse, dense}, {1, 0});
tensor format d1d0 ({dense, dense}, {1, 0});
tensor format s ({sparse});

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
  tensor<s1d0> A = build(tensor<s1d0>)({4, 4});
  tensor<d1d0> B = build(tensor<d1d0>)({4, 4});
  tensor<s> C = build(tensor<s>)({4});

  B[0,1] = 0.5;
  B[0,2] = 1.0;
  B[1,0] = 1.5;
  B[1,2] = 2.0;
  B[1,3] = 2.5;
  B[3,0] = 0.5;

  C[0] = 0.5;
  C[1] = 1.0;
  C[2] = 2.0;
  C[3] = 0.25;

  A[i,j] = B[i,j] / C[j];

  assert(A[0,0], 0.0);
  assert(A[0,1], 0.5);
  assert(A[0,2], 0.5);
  assert(A[0,3], 0.0);
  assert(A[1,0], 3.0);
  assert(A[1,1], 0.0);
  assert(A[1,2], 1.0);
  assert(A[1,3], 10.0);
  assert(A[2,0], 0.0);
  assert(A[2,1], 0.0);
  assert(A[2,2], 0.0);
  assert(A[2,3], 0.0);
  assert(A[3,0], 1.0);
  assert(A[3,1], 0.0);
  assert(A[3,2], 0.0);
  assert(A[3,3], 0.0);

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  if(error) exit(1);

  return 0;
}
