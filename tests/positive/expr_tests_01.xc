#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});
tensor format vec ({sparse});

indexvar i, j, k, l;

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0.0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

double data0[8][8] =
{
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
  {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
};

double data1[8] = {0.0, 3.0, 0.0, 2.0, 5.0, 5.0, 0.0, 3.0};

double data2[8] = {3.0, 7.0, 5.0, 3.0, 3.0, 7.0, 3.0, 3.0};

int main() {
  tensor<mat> A = build(tensor<mat>)({8, 8});
  tensor<mat> B = build(tensor<mat>)({8, 8}); 
  tensor<mat> C = build(tensor<mat>)({8, 8}); 
  tensor<mat> D = build(tensor<mat>)({8, 8}); 

  B[1,2] = 2.0;
  C[1,3] = 3.0;
  D[2,3] = 0.5;

  A[i,j] = B[i,k] * C[i,j] * D[k,l];

  for(int i = 0; i < 8; i++) {
    for(int j = 0; j < 8; j++) {
      assert(A[i, j], data0[i][j]);
    }
  }

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
  for(int i = 0; i < 8; i++) {
    assert(a[i], data1[i]);
  }

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

  for(int i = 0; i < 8; i++) {
    assert(z[i], data2[i]);
  }

  freeTensor(X);
  freeTensor(y);
  freeTensor(z);

  if(error) exit(1);
  return 0;
}
