#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});

indexvar i, j, k, l;

int main() {
  tensor<mat> A = build(tensor<mat>)({8, 8});
  tensor<mat> B = build(tensor<mat>)({8, 8}); 
  tensor<mat> C = build(tensor<mat>)({8, 8}); 
  tensor<mat> D = build(tensor<mat>)({8, 8}); 

  B[1,2] = 2.0;
  C[1,3] = 3.0;
  D[2,3] = 0.5;

  A[i,j] = B[i,k] * C[i,j] * D[k,l];

  printf("A[1,3] = %f (should be %f)\n", A[1,3], 3.0);
}
