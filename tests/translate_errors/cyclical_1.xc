#include "tensors.xh"

tensor format s0s1 ({sparse, sparse}, {0, 1});
tensor format s1s0 ({sparse, sparse}, {1, 0});

indexvar i, j;

int main() {
  tensor<s0s1> A = build(tensor<s0s1>)({5, 5});
  tensor<s0s1> B = build(tensor<s0s1>)({5, 5});
  tensor<s1s0> C = build(tensor<s1s0>)({5, 5});

  A[i, j] = B[i, j] + C[i, j];

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  return 0;
}
