#include "tensors.xh"
#include <stdio.h>

tensor format vec ({dense});
tensor format mat ({dense, dense});

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
  tensor<vec> v = build(tensor<vec>)([1, 2, 3, 4, 5, 6, 7, 8]);
  tensor<mat> m = build(tensor<mat>)([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);

  assert(v[0], 1.0);
  assert(v[1], 2.0);
  assert(v[2], 3.0);
  assert(v[3], 4.0);
  assert(v[4], 5.0);
  assert(v[5], 6.0);
  assert(v[6], 7.0);
  assert(v[7], 8.0);

  assert(m[0,0], 1.0);
  assert(m[0,1], 2.0);
  assert(m[0,2], 3.0);
  assert(m[1,0], 4.0);
  assert(m[1,1], 5.0);
  assert(m[1,2], 6.0);
  assert(m[2,0], 7.0);
  assert(m[2,1], 8.0);
  assert(m[2,2], 9.0);

  freeTensor(v);
  freeTensor(m);

  if(error) exit(1);
  return 0;
}
