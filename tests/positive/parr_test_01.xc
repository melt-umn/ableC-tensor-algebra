#define TACO_PARALLEL
#define TACO_THREADS 6

#include "tensors.xh"
#include <stdlib.h>
#include <stdio.h>

tensor format mat ({sparse, dense});
tensor format mtx ({sparse, dense}, {1, 0});
tensor format vec ({sparse});

indexvar i, j, k, l;

int main() {
  tensor<mat> out = build(tensor<mat>)({100, 250});
  tensor<mat> ref = build(tensor<mat>)({100, 250});
  tensor<mtx> A = build(tensor<mtx>)({1000, 250});
  tensor<mat> B = build(tensor<mat>)({100, 1000});

  for(int c = 0; c < 1000; c++) {
    int Ak = rand() % 1000;
    int Aj = rand() % 250;
    int Bi = rand() % 100;
    int Bk = rand() % 1000;

    A[Ak, Aj] = ((double)rand()) / RAND_MAX;
    B[Bi, Bk] = ((double)rand()) / RAND_MAX;
  }

  out[i,j] = A[k,j] * B[i,k];
  ref[i,j] = A[k,j] * B[i,k];

  char error = 0;
  for(int i = 0; i < 100; i++)
    for(int j = 0; j < 250; j++)
      if(out[i,j] != ref[i,j]) exit(1);

  return 0;
}
