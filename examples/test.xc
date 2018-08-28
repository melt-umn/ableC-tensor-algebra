#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});
tensor format vec ({sparse});

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

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
  freeTensor(D);

  tensor<vec> a = build(tensor<vec>)({8});
  tensor<vec> b = build(tensor<vec>)({8});
  tensor<vec> c = build(tensor<vec>)({8});
  tensor<vec> d = build(tensor<vec>)({8});

  b[1] = 1.0;
  b[3] = 2.0;
  b[7] = 3.0;

  c[1] = 2.0;
  c[4] = 3.0;

  d[4] = 2.0;
  d[5] = 5.0;

  a[i] = b[i] + (c[i] + d[i]);
  printf("\n");
  for(int i = 0; i < 8; i++)
    printf("%2.2f ", a[i]);
  printf("\n");
  printf("0.00 3.00 0.00 2.00 5.00 5.00 0.00 3.00\n");

  freeTensor(a);
  freeTensor(b);
  freeTensor(c);
  freeTensor(d);

  tensor<mat> X = build(tensor<mat>)({8, 8});
  tensor<vec> y = build(tensor<vec>)({8});
  tensor<vec> z = build(tensor<vec>)({8});

  X[0,1] = 1.0;
  X[1,2] = 2.0;
  X[3,1] = 3.0;
  X[7,5] = 4.0;

  y[0] = 1.0;
  y[4] = 2.0;

  z[j] = X[i,j] + y[i];

  printf("\n");
  for(int i = 0; i < 8; i++)
    printf("%2.2f ", z[i]);
  printf("\n");
  printf("3.00 7.00 5.00 3.00 3.00 7.00 3.00 3.00\n");

  freeTensor(X);
  freeTensor(y);
  freeTensor(z);

  return 0;
}
