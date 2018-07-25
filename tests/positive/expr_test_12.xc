#include "tensors.xh"

// formats
tensor format ts3 ({sparse, dense, sparse});

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
  tensor<ts3> A = build(tensor<ts3>)({2, 3, 2});
  tensor<ts3> B = build(tensor<ts3>)({2, 3, 2});
  tensor<ts3> C = build(tensor<ts3>)({2, 3, 2});

  // initialize tensors
  B[0, 1, 1] = 0.5;
  B[1, 2, 0] = 1.0;
  B[0, 2, 0] = 1.5;

  C[1, 1, 1] = 0.5;
  C[1, 2, 0] = 1.0;
  C[0, 2, 0] = 1.5;

  // tensor expression
  A[i, j, k] = B[i,j,k] + C[i,j,k];

  // asserts
  assert(A[0,0,0], 0.0);
  assert(A[0,0,1], 0.0);
  assert(A[0,1,0], 0.0);
  assert(A[0,1,1], 0.5);
  assert(A[0,2,0], 3.0);
  assert(A[0,2,1], 0.0);
  assert(A[1,0,0], 0.0);
  assert(A[1,0,1], 0.0);
  assert(A[1,1,0], 0.0);
  assert(A[1,1,1], 0.5);
  assert(A[1,2,0], 2.0);
  assert(A[1,2,1], 0.0);

  if(error) exit(1);
  return 0;
}
