#include "tensors.xh"

tensor format s0 ({sparse}, {0});

indexvar i;

int main() {
  tensor<s0> a = build(tensor<s0>)({3});
  tensor<s0> b = build(tensor<s0>)({5});

  a[i] = c[i];

  freeTensor(a);
  freeTensor(b);
  
  return 0;
}
