#include "tensors.xh"
#include <stdio.h>

tensor format s1s0 ({sparse, sparse}, {1, 0});
tensor format s1s2d0 ({sparse, sparse, dense}, {1, 2, 0});
tensor format s0d1 ({sparse, dense}, {0, 1});
tensor format d0d1 ({dense, dense}, {0, 1});

indexvar i, j, k, l;

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
  tensor<s1s0> A = build(tensor<s1s0>)({4, 4});
  tensor<s1s2d0> B = build(tensor<s1s2d0>)({4, 4, 4});
  tensor<s0d1> C = build(tensor<s0d1>)({4, 4});
  tensor<d0d1> D = build(tensor<d0d1>)({4, 4});

  B[0,0,0] = 2.0;
  B[0,3,0] = 0.5;
  B[1,3,1] = 1.0;
  B[2,3,3] = 2.0;
  B[3,1,0] = 2.0;
  B[3,3,1] = 1.0;

  C[0,1] = 0.5;
  C[0,2] = 1.0;
  C[1,0] = 1.5;
  C[1,2] = 2.0;
  C[1,3] = 2.5;
  C[3,0] = 0.5;

  D[0,1] = 0.5;
  D[1,1] = 1.0;
  D[1,2] = 1.5;
  D[3,0] = 2.0;
  D[3,1] = 2.5;
  D[3,3] = 0.5;

  A[i,j] = B[i,k,l] * C[l,j] * D[k,j];

  assert(A[0,0], 0.0);
  assert(A[0,1], 1.125);
  assert(A[0,2], 0.0);
  assert(A[0,3], 0.0);
  assert(A[1,0], 3.0);
  assert(A[1,1], 0.0);
  assert(A[1,2], 0.0);
  assert(A[1,3], 1.25);
  assert(A[2,0], 2.0);
  assert(A[2,1], 0.0);
  assert(A[2,2], 0.0);
  assert(A[2,3], 0.0);
  assert(A[3,0], 3.0);
  assert(A[3,1], 1.0);
  assert(A[3,2], 3.0);
  assert(A[3,3], 1.25);

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
  freeTensor(D);

  if(error) exit(1);

  return 0;
}
