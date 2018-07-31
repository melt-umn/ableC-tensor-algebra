#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "tensors.xh"

#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 5
#define VECTOR_SIZE 8

#define M 3000
#define N 5000
#define P 7000

tensor format mat ({dense, dense});

indexvar i, j, k;

int main() {
  srand(1234);
  
  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N});
  tensor<mat> C = build(tensor<mat>)({M, N});
  tensor<mat> D = build(tensor<mat>)({M, N});

  for(int c = 0; c < 100000; c++) {
    int i = rand() % M;
    int k = rand() % P;
    A[i,k] = ((double)rand()) / RAND_MAX;
  }
  for(int c = 0; c < 100000; c++) {
    int k = rand() % P;
    int j = rand() % N;
    B[k,j] = ((double)rand()) / RAND_MAX;
  }

  struct timeval start, end;

  fprintf(stderr, "Performing matrix multiplication... ");
  gettimeofday(&start, NULL);
  tensor transform {
    C[i,j] = A[i,k] * B[k,j];
  } by {
    parallelize i;
  }
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n",
    (double)(end.tv_usec - start.tv_usec) / 1000000 +
    (double)(end.tv_sec - start.tv_sec));

  fprintf(stderr, "Performing reference matrix multiplication... ");
  gettimeofday(&start, NULL);
  D[i,j] = A[i,k] * B[k, j];
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n",
    (double)(end.tv_usec - start.tv_usec) / 1000000 +
    (double)(end.tv_sec - start.tv_sec));

  char error = 0;
  for(int i = 0; i < M; i++) {
    for(int j = 0; j < N; j++) {
      if(C[i,j] != D[i,j])
        error = 1;
    }
  }
  if(error)
    fprintf(stderr, "Error...\n");
  else
    fprintf(stderr, "Success!\n");

  return 0;
}
