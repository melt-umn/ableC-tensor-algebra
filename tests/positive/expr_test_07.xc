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
  tensor<mat> A = build(tensor<mat>)({2, 2});
  tensor<mat> B = build(tensor<mat>)({2, 3});
  tensor<mat> C = build(tensor<mat>)({3, 2});

  // initialize tensors
  B[0, 0] = 1.0;
  B[0, 1] = 3.0;
  B[1, 2] = 1.0;

  C[0, 0] = 1.0;
  C[1, 0] = 2.0;
  C[1, 1] = 1.0;

  // tensor expression
  A[i, j] = B[i, k] * C[k, j];

  // asserts
  assert(A[0, 0], 7.0);
  assert(A[0, 1], 3.0);
  assert(A[1, 0], 0.0);
  assert(A[1, 1], 0.0);

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  if(error) exit(1);
  return 0;
}
