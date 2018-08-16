#include "tensors.xh"

tensor format mat ({sparse, sparse});

indexvar i, j, k;

int main() {
  tensor<mat> A = build(tensor<mat>)({100, 100});
  tensor<mat> B = build(tensor<mat>)({100, 100});
  tensor<mat> C = build(tensor<mat>)({100, 100});

  C[i, j] = A[i, k] * B[k, j];

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
}
