#include "tensors.xh"

// formats
tensor format vec ({sparse});
tensor format mat ({dense, sparse});

// index variables
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
  // declare tensors
  tensor<vec> a = build(tensor<vec>)({8});
  tensor<mat> B = build(tensor<mat>)({8, 6});
  tensor<mat> C = build(tensor<mat>)({8, 6});
  tensor<vec> d = build(tensor<vec>)({6});

  double alpha = 1.5;

  // initialize tensors
  B[0, 2] = 0.5;
  B[0, 4] = 1.0;
  B[2, 2] = 1.5;
  B[3, 3] = 2.0;
  B[7, 1] = 2.5;
  B[7, 3] = 3.0;

  C[0, 2] = 0.5;
  C[0, 4] = 1.0;
  C[3, 3] = 1.5;
  C[7, 3] = 2.0;

  d[1] = 0.5;
  d[2] = 1.0;
  d[3] = 1.5;

  // tensor expression
  a[i] = alpha * (B[i,j] + C[i,j]) * d[j];

  // asserts
  assert(a[0], 1.5);
  assert(a[1], 0.0);
  assert(a[2], 2.25);
  assert(a[3], 7.875);
  assert(a[4], 0.0);
  assert(a[5], 0.0);
  assert(a[6], 0.0);
  assert(a[7], 13.125);

  if(error) exit(1);
  return 0;
}
