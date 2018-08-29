#include "tensors.xh"
#include <stdio.h>

tensor format vec ({sparse});
indexvar i;

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0.0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

int main() {
  tensor<vec> a = build(tensor<vec>)({10});
  tensor<vec> b = build(tensor<vec>)({10});

  b[0] = 1.0;
  b[2] = 2.0;
  b[6] = 3.0;
  b[4] = 4.0;

  a[i] = 2 * b[i] + 1;

  assert(a[0], 3.0);
  assert(a[1], 1.0);
  assert(a[2], 5.0);
  assert(a[3], 1.0);
  assert(a[4], 9.0);
  assert(a[5], 1.0);
  assert(a[6], 7.0);
  assert(a[7], 1.0);
  assert(a[8], 1.0);
  assert(a[9], 1.0);

  freeTensor(a);
  freeTensor(b);

  if(error) exit(1);
  return 0;
}
