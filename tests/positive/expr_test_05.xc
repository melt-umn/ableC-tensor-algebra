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
  tensor<vec> a = build(tensor<vec>)({10});
  tensor<mat> B = build(tensor<mat>)({10, 12});
  tensor<vec> c = build(tensor<vec>)({12});

  // initialize tensors
  B[0, 1] = 1.0;
  B[0, 3] = 2.0;
  B[3, 2] = 3.0;
  B[7, 2] = 4.0;
  B[7, 4] = 5.0;
  B[8, 4] = 6.0;

  c[1] = 7.0;
  c[2] = 8.0;
  c[7] = 9.0;

  // tensor expression
  a[i] = B[i, j] * c[j];

  // asserts
  assert(a[0],  7.0);
  assert(a[1],  0.0);
  assert(a[2],  0.0);
  assert(a[3], 24.0);
  assert(a[4],  0.0);
  assert(a[5],  0.0);
  assert(a[6],  0.0);
  assert(a[7], 32.0);
  assert(a[8],  0.0);
  assert(a[9],  0.0);

  if(error) exit(1);
  return 0;
}
