#define TACO_PARALLEL
#define TACO_THREADS 6

#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

#include <omp.h>

tensor format vec ({dense});
tensor format mat ({dense, dense});

tensor format ts3 ({sparse, sparse, sparse});
tensor format vct ({sparse});
tensor format out ({dense, dense});

indexvar i, j, k;

int main() {
  tensor<vec> a0 = build(tensor<vec>)({500});
  tensor<vec> a1 = build(tensor<vec>)({500});
  tensor<mat> b = build(tensor<mat>)({500, 500});

  srand(1234);
  for(int c = 0; c < 1000; c++) {
    int i = rand() % 500;
    int j = rand() % 500;
    b[i, j] = ((double)rand()) / RAND_MAX;
  }

  a0[i] = b[j,i];

  a1[i] = b[j,i];

  char error = 0;
  for(int i = 0; i < 500; i++) {
    if(a0[i] != a1[i]) {
      //fprintf(stderr, "Mismatch at index %d\n", i);
      error = 1;
    }
  }
  if(error)
    fprintf(stderr, "Fail...\n");
  else
    fprintf(stderr, "Success!\n");

  freeTensor(a0);
  freeTensor(a1);
  freeTensor(b);

  tensor<ts3> X = build(tensor<ts3>)({100, 100, 100});
  tensor<vct> y = build(tensor<vct>)({100});
  tensor<out> z = build(tensor<out>)({100, 100});
  tensor<out> q = build(tensor<out>)({100, 100});

  for(int c = 0; c < 1000; c++) {
    int i = rand() % 100;
    int j = rand() % 100;
    int k = rand() % 100;
    X[i,k,j] = ((double)rand()) / RAND_MAX;
  }
  for(int c = 0; c < 30; c++) {
    int i = rand() % 100;
    y[i] = ((double)rand()) / RAND_MAX;
  }

  z[i,j] = X[i,k,j] + y[i];
  q[i,j] = X[i,k,j] + y[i];

  error = 0;
  for(int i = 0; i < 100; i++) {
    for(int j = 0; j < 100; j++) {
      if(z[i,j] != q[i,j]) {
        error = 1;
      }
    }
  }
  if(error)
    fprintf(stderr, "Fail...\n");
  else
    fprintf(stderr, "Success!\n");

  freeTensor(X);
  freeTensor(y);
  freeTensor(z);
  freeTensor(q);

  return 0;
}
