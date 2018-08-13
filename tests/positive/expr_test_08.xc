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
  tensor<mat> B = build(tensor<mat>)({10, 8});
  tensor<vec> c = build(tensor<vec>)({8});

  // initialize tensors
  B[2, 3] = 0.5;
  B[2, 6] = 1.0;
  B[4, 7] = 1.5;
  B[5, 3] = 2.0;
  B[5, 6] = 2.5;
  B[5, 7] = 3.0;

  c[2] = 3.5;
  c[3] = 4.0;
  c[6] = 4.5;

  // tensor expression
  a[i] = B[i, j] * c[j];

  // asserts
  assert(a[0], 0.0);
  assert(a[1], 0.0);
  assert(a[2], 6.5);
  assert(a[3], 0.0);
  assert(a[4], 0.0);
  assert(a[5], 19.25);
  assert(a[6], 0.0);
  assert(a[7], 0.0);
  assert(a[8], 0.0);
  assert(a[9], 0.0);

  freeTensor(a);
  freeTensor(B);
  freeTensor(c);

  if(error) exit(1);
  return 0;
}
