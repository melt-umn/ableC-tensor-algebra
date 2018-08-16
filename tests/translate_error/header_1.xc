#include <stdlib.h>

tensor format mat ({dense, sparse});

indexvar i, j;

int main() {
  tensor<mat> A = build(tensor<mat>)({50, 50});
  tensor<mat> B = build(tensor<mat>)({50, 50});

  A[i,j] = B[i,j];

  freeTensor(A);
  freeTensor(B);

  return 0;
}
