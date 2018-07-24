#include "tensors.xh"

tensor format s ({sparse});

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
  tensor<s> a = build(tensor<s>)({8});
  tensor<s> b = build(tensor<s>)({8});
  b[0] = 10.0;
  b[2] = 20.0;
  b[3] = 30.0;

  a[i] = b[i] + b[i];

  assert(a[0], 20.0);
  assert(a[1],  0.0);
  assert(a[2], 40.0);
  assert(a[3], 60.0);
  assert(a[4],  0.0);
  assert(a[5],  0.0);
  assert(a[6],  0.0);
  assert(a[7],  0.0);

  if(error) exit(1);
  return 0;
}
