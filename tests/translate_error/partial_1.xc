#include "tensors.xh"

tensor format vec ({sparse});
tensor format mat ({sparse, sparse});

indexvar i;

int main() {
  tensor<vec> a = build(tensor<vec>)({10});
  tensor<mat> B = build(tensor<mat>)({10, 5});
  tensor<vec> c = build(tensor<vec>)({5});

  a[i] = B[i,j] + c[j];

  freeTensor(a);
  freeTensor(B);
  freeTensor(c);

  return 0;
}
