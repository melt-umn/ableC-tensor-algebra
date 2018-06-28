#include "tensors.xh"

tensor format s0 = ({sparse}, {0});
tensor format s0s1 = ({sparse, sparse}, {0, 1});

int main() {
  tensor<s0> a = build (tensor<s0>)({5});
  tensor<s0s1> B = build (tensor<s0s1>)({5, 4});
  tensor<s0> c = build (tensor<s0>)({3});

  tensor a(i) = B(i, j) * c(j);
  
  return 0;
}
