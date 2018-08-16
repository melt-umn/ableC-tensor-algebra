#include "tensors.xh"

tensor format s0 ({sparse});
tensor format s0s1 ({sparse, sparse});

indexvar i, j;

int main() {
  tensor<s0s1> A = build(tensor<s0s1>)({5, 5});
  tensor<s0> b = build(tensor<s0>)({5});

  A[i,j] = b[i];

  freeTensor(A);
  freeTensor(b);

  return 0;
}
