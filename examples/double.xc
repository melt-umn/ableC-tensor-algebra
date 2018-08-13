#include <stdio.h>
#include "tensors.xh"

tensor format vec ({sparse});
indexvar i;

int main() {
  tensor<vec> a = build(tensor<vec>)({8});
  tensor<vec> b = build(tensor<vec>)({8});

  b[0] = 10.0;
  b[2] = 20.0;
  b[3] = 30.0;

  a[i] = b[i] + b[i];

  printf("a[%d] = %f (should be %f)\n", 0, a[0], 20.0);
  printf("a[%d] = %f (should be %f)\n", 2, a[2], 40.0);
  printf("a[%d] = %f (should be %f)\n", 3, a[3], 60.0);

  freeTensor(a);
  freeTensor(b);

  return 0;
}
