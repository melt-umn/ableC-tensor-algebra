#include "tensors.xh"

tensor format s0s1s2 = ({sparse, sparse, sparse}, {0, 1, 2});

int main() {
  tensor<s0s1s2> A = build (tensor<s0s1s2>)({5, 5, 5});
  tensor<s0s1s2> B = build (tensor<s0s1s2>)({5, 5, 5});
  tensor<s0s1s2> C = build (tensor<s0s1s2>)({5, 5, 5});
  
  tensor A(i, j, k) = B(i, j, k) + C(k, i, j);
  
  return 0;
}
