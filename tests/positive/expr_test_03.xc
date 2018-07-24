#include "tensors.xh"

tensor format vec ({dense});

indexvar i;

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
  tensor<vec> a = build(tensor<vec>)({13});
  tensor<vec> b = build(tensor<vec>)({13});
  tensor<vec> c = build(tensor<vec>)({13});

  b[2] = 1.0;
  b[6] = 2.0;
  b[11] = 6.0;
  c[2] = 3.0;
  c[7] = 5.0;
  c[11] = -2.0;

  a[i] = b[i] + c[i];

  assert(a[ 0], 0.0);
  assert(a[ 1], 0.0);
  assert(a[ 2], 4.0);
  assert(a[ 3], 0.0);
  assert(a[ 4], 0.0);
  assert(a[ 5], 0.0);
  assert(a[ 6], 2.0);
  assert(a[ 7], 5.0);
  assert(a[ 8], 0.0);
  assert(a[ 9], 0.0);
  assert(a[10], 0.0);
  assert(a[11], 4.0);
  assert(a[12], 0.0);

  if(error) exit(1);
  return 0;
}
