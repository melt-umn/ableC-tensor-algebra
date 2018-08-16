#include "tensors.xh"

tensor format s0 ({sparse}, {0});
tensor format s0s1 ({sparse, sparse}, {0, 1});

indexvar i, j;

int main() {
  tensor<s0> a = build(tensor<s0>)({5});
  tensor<s0s1> B = build(tensor<s0s1>)({5, 4});
  tensor<s0> c = build(tensor<s0>)({3});

  a[i] = B[i, j] * c[j];

  freeTensor(a);
  freeTensor(B);
  freeTensor(c);
  
  return 0;
}
