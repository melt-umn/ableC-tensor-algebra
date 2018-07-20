#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});
tensor format vec ({sparse});

indexvar i, j, k;

int main() {
  tensor<mat> A = build(tensor<mat>)({4, 16 / 4});

  A[0,0] = 2.0;
  A[0,0] *= 3.0;
  A[1,3] = 1.0;
  A[1,3] += 4.0;
  A[2,2] = 1.5;
  A[2,2] /= 3.0;
  A[2,3] = 3.0;
  A[3,0] = 1.2;
  A[3,0] -= 0.2;
  A[3,2] = 1.5;

  for(int i = 0; i < (dimenof(A))[0]; i++) {
    for(int j = 0; j < (dimenof(A))[1]; j++) {
      printf("A[%d, %d] = %f\n", i, j, A[i,j]);
    }
  }

  tensor<vec> b = build(tensor<vec>)({8});
  
  b[0] = 1.0;
  b[2] = 1.5;
  b[2] *= 2.0;
  b[5] = 3.5;
  b[5] /= 7;
  b[7] = 2.5;
  
  for(int i = 0; i < (dimenof(b))[0]; i++) {
    printf("b[%d] = %f\n", i, b[i]);
  }
  
  double x = 6.0;
  
  x *= 2.0;
  x /= 4.0;
  
  printf("x = %f\n", x);

  tensor<vec> B = build(tensor<vec>)({7});
  tensor<mat> C = build(tensor<mat>)({7, 7});

  double y;

  B[i] = 4 * A[i,j] + b[i] * x;
  y = 4 * A[i,j] + b[i] * x;

  return 0;
}
