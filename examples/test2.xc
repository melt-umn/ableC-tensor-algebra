#include "tensors.xh"
#include <stdio.h>

tensor format vec ({dense});

indexvar i, j, k;

int main() {
  tensor<vec> a = build(tensor<vec>)({6});
  tensor<vec> b = build(tensor<vec>)({6});
  tensor<vec> c = build(tensor<vec>)({6});
  tensor<vec> d = build(tensor<vec>)({6});

  a[0] = 1.0;
  a[1] = 0.5;
  a[2] = 2.0;
  a[5] = 3.0;

  b[1] = 1.0;
  b[3] = 3.0;
  b[5] = 6.0;

  c[0] = 1.0;
  c[2] = 1.0;
  c[4] = 1.0;
  c[5] = 1.0;

  d[i] = a[i] * (b[j] + c[k]);

  for(int i = 0; i < 6; i++)
    printf("%2.2f ", d[i]);
  printf("\n");
  printf("14.00 7.00 28.00 0.00 0.00 42.00\n");
}
