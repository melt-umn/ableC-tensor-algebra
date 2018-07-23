#include <stdio.h>
#include "tensors.xh"

tensor format den ({dense, dense});
tensor format mat ({dense, sparse});
tensor format vec ({sparse});

indexvar i, j, k;

int main() {
  tensor<den> A = build(tensor<den>)({4, 16 / 4});
  tensor<mat> B = build(tensor<mat>)({4, 4});
  tensor<mat> C = build(tensor<mat>)({4, 4});

  A[i,j] = B[i,j] + C[j,k];

  return 0;
}
