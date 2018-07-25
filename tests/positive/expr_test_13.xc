#include "tensors.xh"

// formats
tensor format mat ({dense, sparse});
tensor format ts4 ({sparse, dense, sparse, sparse});

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
  tensor<mat> a = build(tensor<mat>)({4, 5});
  tensor<ts4> B = build(tensor<ts4>)({4, 3, 5, 3});
  tensor<mat> c = build(tensor<mat>)({3, 3});

  // initialize tensors
  B[0,0,0,0] = 0.5;
  B[0,1,2,1] = 1.0;
  B[1,2,1,2] = 1.5;
  B[1,1,1,0] = 2.0;
  B[1,2,3,2] = 2.5;

  c[0,0] = 0.5;
  c[2,2] = 1.0;
  c[1,0] = 1.5;
  c[2,0] = 2.0;

  // tensor expression
  a[i,k] = B[i,j,k,l] * c[j,l];

  // asserts
  assert(a[0,0], 0.25);
  assert(a[0,1], 0.0);
  assert(a[0,2], 0.0);
  assert(a[0,3], 0.0);
  assert(a[0,4], 0.0);
  assert(a[1,0], 0.0);
  assert(a[1,1], 4.5);
  assert(a[1,2], 0.0);
  assert(a[1,3], 2.5);
  assert(a[1,4], 0.0);
  assert(a[2,0], 0.0);
  assert(a[2,1], 0.0);
  assert(a[2,2], 0.0);
  assert(a[2,3], 0.0);
  assert(a[2,4], 0.0);
  assert(a[3,0], 0.0);
  assert(a[3,1], 0.0);
  assert(a[3,2], 0.0);
  assert(a[3,3], 0.0);
  assert(a[3,4], 0.0);

  if(error) exit(1);
  return 0;
}
