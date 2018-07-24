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
  tensor<mat> B = build(tensor<mat>)({8, 5});
  tensor<vec> c = build(tensor<vec>)({5});
  tensor<vec> d = build(tensor<vec>)({8});

  double alpha = 2.0;
  double beta = 0.5;

  // initialize tensors
  B[1, 3] = 0.5;
  B[3, 3] = 1.0;
  B[3, 4] = 1.5;
  B[4, 2] = 2.0;
  B[7, 1] = 2.5;
  B[7, 2] = 3.0;

  c[2] = 0.5;
  c[3] = 1.0;
  c[4] = 1.5;

  d[1] = 0.5;
  d[2] = 1.0;

  // tensor expression
  a[i] = alpha * B[i, j] * c[j] + beta * d[i];

  // asserts
  assert(a[0], 0.0);
  assert(a[1], 1.25);
  assert(a[2], 0.5);
  assert(a[3], 6.5);
  assert(a[4], 2.0);
  assert(a[5], 0.0);
  assert(a[6], 0.0);
  assert(a[7], 3.0);

  if(error) exit(1);
  return 0;
}
