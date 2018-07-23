#include <stdio.h>
#include "tensors.xh"

tensor format fC ({sparse});
tensor format fB ({sparse, sparse, sparse});
tensor format fA ({dense, sparse});

indexvar i, j, k;

int main() {

  tensor<fA> A = build(tensor<fA>)({64, 42});
  tensor<fB> B = build(tensor<fB>)({64, 42, 512});
  tensor<fC> C = build(tensor<fC>)({512});

  B[0, 0, 0] = 1.0;
  B[1, 2, 0] = 2.0;
  B[1, 2, 1] = 3.0;

  C[0] = 4.0;
  C[1] = 5.0;

  printf("B[0, 0, 0] = %f\n", B[0, 0, 0]);
  printf("B[1, 2, 0] = %f\n", B[1, 2, 0]);
  printf("B[1, 2, 1] = %f\n", B[1, 2, 1]);

  A[i,j] = B[i,j,k] * C[k];

  printf("A[0, 0] = %f (should be 4.0)\n", A[0, 0]);
  printf("A[1, 2] = %f (should be 23.0)\n", A[1, 2]);

  return 0;
}
