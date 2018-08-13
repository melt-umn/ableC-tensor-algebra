#include "tensors.xh"

// formats
tensor format mat ({dense, sparse});
tensor format ts3 ({sparse, sparse, sparse});

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
  tensor<ts3> A = build(tensor<ts3>)({2, 2, 3});
  tensor<ts3> B = build(tensor<ts3>)({2, 2, 4});
  tensor<mat> C = build(tensor<mat>)({3, 4});

  // initialize tensors
  B[0, 0, 0] = 0.5;
  B[0, 1, 2] = 1.0;
  B[1, 1, 1] = 1.5;
  B[1, 1, 3] = 2.0;

  C[1, 2] = 0.5;
  C[1, 3] = 1.0;
  C[2, 0] = 1.5;

  // tensor expression
  A[i, j, k] = B[i, j, l] * C[k, l];

  // asserts
  assert(A[0,0,0], 0.0);
  assert(A[0,0,1], 0.0);
  assert(A[0,0,2], 0.75);
  assert(A[0,1,0], 0.0);
  assert(A[0,1,1], 0.5);
  assert(A[0,1,2], 0.0);
  assert(A[1,0,0], 0.0);
  assert(A[1,0,1], 0.0);
  assert(A[1,0,2], 0.0);
  assert(A[1,1,0], 0.0);
  assert(A[1,1,1], 2.0);
  assert(A[1,1,2], 0.0);

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
  
  if(error) exit(1);
  return 0;
}
