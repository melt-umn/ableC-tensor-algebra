#include "tensors.xh"

tensor format s0 = ({sparse}, {0});
tensor format s0s1 = ({sparse, sparse}, {1, 0});

int main() {
  tensor<s0> A = build (tensor<s0>)({5});
  tensor<s0s1> B = build (tensor<s0s1>)({5, 5});
  tensor<s0s1> C = build (tensor<s0s1>)({5, 5});
  
  tensor A(i) = B(j) + C(i, j);
  
  return 0;
}
