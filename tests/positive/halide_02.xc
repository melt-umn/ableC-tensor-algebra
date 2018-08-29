#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 5
#define VECTOR_SIZE 8

#define M 300
#define N 500
#define P 700

tensor format mat ({dense, dense});

indexvar i, j, k;

int main() {
  srand(1234);
  
  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N});
  tensor<mat> C = build(tensor<mat>)({M, N});
  tensor<mat> D = build(tensor<mat>)({M, N});

  for(int c = 0; c < 1000; c++) {
    int i = rand() % M;
    int k = rand() % P;
    A[i,k] = ((double)rand()) / RAND_MAX;
  }
  for(int c = 0; c < 1000; c++) {
    int k = rand() % P;
    int j = rand() % N;
    B[k,j] = ((double)rand()) / RAND_MAX;
  }

  tensor transform {
    C[i,j] = A[i,k] * B[k,j];
  } by {
    //parallelize i;
  }

  D[i,j] = A[i,k] * B[k, j];

  char error = 0;
  for(int i = 0; i < M; i++) {
    for(int j = 0; j < N; j++) {
      if(C[i,j] != D[i,j]) {
        fprintf(stderr, "Error at (%d, %d). C = %f, D = %f\n", i, j, C[i,j], D[i, j]);
        error = 1;
      }
    }
  }

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
  freeTensor(D);

  if(error) {
    exit(1);
  }
  
  return 0;
}
