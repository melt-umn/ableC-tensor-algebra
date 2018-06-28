#include "tensors.xh"

tensor format s0 = ({sparse}, {0});

int main() {
  tensor<s0> a = build (tensor<s0>)({3});
  tensor<s0> b = build (tensor<s0>)({5});
  
  tensor a(i) = b(i);
  
  return 0;
}
