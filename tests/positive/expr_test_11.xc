#include "tensors.xh"

// formats
tensor format mat ({dense, sparse});

// index variables
indexvar i, j, k;

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
  // declare tensors
  tensor<mat> A = build(tensor<mat>)({3, 4});
  tensor<mat> B = build(tensor<mat>)({3, 4});
  tensor<mat> C = build(tensor<mat>)({3, 5});
  tensor<mat> D = build(tensor<mat>)({5, 4});

  // initialize tensors
  B[0, 0] = 0.5;
  B[1, 2] = 1.0;
  B[2, 3] = 1.5;

  C[0, 2] = 0.5;
  C[0, 4] = 1.0;
  C[1, 2] = 1.5;
  C[2, 1] = 2.0;

  D[1, 3] = 0.5;
  D[2, 0] = 1.0;
  D[2, 2] = 1.5;
  D[4, 2] = 2.0;

  // tensor expression
  A[i, j] = B[i,j] * (C[i,k] * D[k,j]);

  // asserts
  assert(A[0,0], 0.25);
  assert(A[0,1], 0.0);
  assert(A[0,2], 0.0);
  assert(A[0,3], 0.0);
  assert(A[1,0], 0.0);
  assert(A[1,1], 0.0);
  assert(A[1,2], 2.25);
  assert(A[1,3], 0.0);
  assert(A[2,0], 0.0);
  assert(A[2,1], 0.0);
  assert(A[2,2], 0.0);
  assert(A[2,3], 1.5);

  if(error) exit(1);
  return 0;
}
