#include "tensors.xh"

// formats
tensor format mat ({dense, dense});
tensor format tns ({sparse, sparse, sparse});

// index variables
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
  // declare tensors
  tensor<mat> A = build(tensor<mat>)({5, 5});
  tensor<tns> B = build(tensor<tns>)({5, 12, 14});
  tensor<mat> C = build(tensor<mat>)({12, 5});
  tensor<mat> D = build(tensor<mat>)({14, 5});

  // initialize tensors
  B[1, 1, 2] = 1.0;
  B[1, 2, 3] = 1.5;
  B[2, 2, 4] = 2.0;
  B[3, 1, 4] = 2.5;
  B[3, 1, 2] = 3.0;

  C[1, 3] = 3.5;
  C[2, 3] = 4.0;
  C[2, 4] = 1.0;
  
  D[2, 3] = 1.5;
  D[3, 3] = 2.0;
  D[4, 3] = 2.5;
  D[4, 4] = 3.0;

  // tensor expression
  A[i, l] = B[i, j, k] * C[j, l] * D[k, l];

  // asserts
  assert(A[0, 0], 0.0);
  assert(A[0, 1], 0.0);
  assert(A[0, 2], 0.0);
  assert(A[0, 3], 0.0);
  assert(A[0, 4], 0.0);
  assert(A[1, 0], 0.0);
  assert(A[1, 1], 0.0);
  assert(A[1, 2], 0.0);
  assert(A[1, 3], 17.25);
  assert(A[1, 4], 0.0);
  assert(A[2, 0], 0.0);
  assert(A[2, 1], 0.0);
  assert(A[2, 2], 0.0);
  assert(A[2, 3], 20.0);
  assert(A[2, 4], 6.0);
  assert(A[3, 0], 0.0);
  assert(A[3, 1], 0.0);
  assert(A[3, 2], 0.0);
  assert(A[3, 3], 37.625);
  assert(A[3, 4], 0.0);
  assert(A[4, 0], 0.0);
  assert(A[4, 1], 0.0);
  assert(A[4, 2], 0.0);
  assert(A[4, 3], 0.0);
  assert(A[4, 4], 0.0);

  if(error) exit(1);
  return 0;
}
