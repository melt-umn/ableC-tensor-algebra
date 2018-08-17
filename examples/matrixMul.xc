#include "tensors.xh"

tensor format mat ({sparse, sparse});

indexvar i, j, k;

int main() {
  tensor<mat> A = build(tensor<mat>)({3, 4});
  tensor<mat> B = build(tensor<mat>)({4, 4});
  tensor<mat> C = build(tensor<mat>)({3, 4});

  A[0,1] = 0.5;
  A[1,2] = 1.0;
  A[1,1] = 1.5;
  A[0,2] = 2.0;

  B[1,1] = 1.5;
  B[1,2] = 2.0;
  B[2,1] = 3.0;
  B[3,1] = 4.0;

  C[i, j] = A[i, k] * B[k, j];

  for(unsigned long i = 0; i < dimenof(C)[0]; i++) {
    for(unsigned long j = 0; j < dimenof(C)[1]; j++) {
      printf("%2.2f ", C[i,j]);
    }
    printf("\n");
  }

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
}
