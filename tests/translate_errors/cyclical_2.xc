#include "tensors.xh"

tensor format s0s1 ({sparse, sparse}, {0, 1});

indexvar i, j;

int main() {
  tensor<s0s1> A = build(tensor<s0s1>)({5, 5});
  tensor<s0s1> B = build(tensor<s0s1>)({5, 5});
  tensor<s0s1> C = build(tensor<s0s1>)({5, 5});

  A[i, j] = B[i, j] + C[j, i];

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  return 0;
}
