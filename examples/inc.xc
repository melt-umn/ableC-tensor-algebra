#include "tensors.xh"
#include <stdio.h>

tensor format vec ({sparse});
indexvar i;

int main() {
  tensor<vec> a = build(tensor<vec>)({10});
  tensor<vec> b = build(tensor<vec>)({10});

  b[0] = 1.0;
  b[2] = 2.0;
  b[6] = 3.0;
  b[4] = 4.0;
  b[9] = 5.0;

  a[i] = 2 * b[i] + 1;

  for(int i = 0; i < 10; i++)
    printf("%2.2f  %2.2f\n", b[i], a[i]);

  freeTensor(a);
  freeTensor(b);

  return 0;
}
