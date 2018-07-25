#include "tensors.xh"

// formats
tensor format mat ({dense, sparse});
tensor format ts3 ({sparse, dense, sparse});

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
  tensor<mat> A = build(tensor<mat>)({3, 4});
  tensor<ts3> B = build(tensor<ts3>)({3, 3, 4});
  tensor<mat> C = build(tensor<mat>)({3, 4});
  tensor<mat> D = build(tensor<mat>)({4, 4});

  // initialize tensors
  B[0,0,0] = 0.5;
  B[0,1,2] = 1.0;
  B[2,0,0] = 1.5;
  B[2,0,2] = 2.0;

  C[0,1] = 0.5;
  C[1,1] = 1.0;
  C[1,2] = 1.5;
  
  D[0,1] = 0.5;
  D[0,2] = 1.0;
  D[2,2] = 1.5;

  // tensor expression
  A[i,j] = B[i,k,l] * C[k,j] * D[l,j];

  // asserts
  assert(A[0,0], 0.0);
  assert(A[0,1], 0.125);
  assert(A[0,2], 2.25);
  assert(A[0,3], 0.0);
  assert(A[1,0], 0.0);
  assert(A[1,1], 0.0);
  assert(A[1,2], 0.0);
  assert(A[1,3], 0.0);
  assert(A[2,0], 0.0);
  assert(A[2,1], 0.375);
  assert(A[2,2], 0.0);
  assert(A[2,3], 0.0);

  if(error) exit(1);
  return 0;
}
