#include "tensors.xh"

tensor format s0 ({sparse});
tensor format s0s1 ({sparse, sparse});

indexvar i, j;

int main() {
  tensor<s0> A = build(tensor<s0>)({5});
  tensor<s0s1> B = build(tensor<s0s1>)({5, 5});
  tensor<s0s1> C = build(tensor<s0s1>)({5, 5});

  A[i] = B[j] + C[i,j];

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  return 0;
}
