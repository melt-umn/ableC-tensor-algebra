#include "tensors.xh"

tensor format s0 = ({sparse}, {0});
tensor format s0s1 = ({sparse, sparse}, {0, 1});

int main() {
  tensor<s0s1> A = build (tensor<s0s1>)({5, 5});
  tensor<s0> b = build (tensor<s0>)({5});
  
  tensor A(i, j) = b(i);
  
  return 0;
}
