#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "tensors.xh"

#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 5
#define VECTOR_SIZE 8

#define M 700
#define N 1000
#define P 2000

tensor format mat ({dense, dense});

indexvar i, j, k;

int main() {
  srand(1234);

  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N}); 
  tensor<mat> C0 = build(tensor<mat>)({M, N}); 
  tensor<mat> C1 = build(tensor<mat>)({M, N});
  tensor<mat> C2 = build(tensor<mat>)({M, N});

  // setup tensors
  fprintf(stderr, "Building input matrices...\n");
  transform {
    for(unsigned k : P) {
      for(unsigned i : M)
        A[i,k] = (double)rand() / (double)RAND_MAX;
      for(unsigned j : M) 
        B[k,j] = (double)rand() / (double)RAND_MAX;
    }
  } by {
    parallelize k;
  }

  struct timeval start, end;

  fprintf(stderr, "Performing matrix multiplication... ");
  gettimeofday(&start, NULL);
  {
  tensor transform {
    C0[i,j] = A[i,k] * B[k,j];
  } by {
    parallelize i into (NUM_THREADS) threads;
  }
  }
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n", 
      (double)(end.tv_usec - start.tv_usec) / 1000000 +
      (double)(end.tv_sec - start.tv_sec));
  
  fprintf(stderr, "Performing reference multiplication... ");
  gettimeofday(&start, NULL);
  {
  tensor transform {
    C1[i,j] = A[i,k] * B[k,j];
  } by {
    
  }
  }
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n", 
      (double)(end.tv_usec - start.tv_usec) / 1000000 +
      (double)(end.tv_sec - start.tv_sec));

  fprintf(stderr, "Performing regular multiplication... ");
  gettimeofday(&start, NULL);
  C2[i,j] = A[i,k] * B[k,j];
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n", 
      (double)(end.tv_usec - start.tv_usec) / 1000000 +
      (double)(end.tv_sec - start.tv_sec));

  fprintf(stderr, "Checking equality... ");
  int error = 0;
  transform {
    for(unsigned i : M, unsigned j : N) {{
      if(C0[i,j] != C1[i,j] || ({double r = C1[i,j] - C2[i,j]; r > 0.001 || r < -0.001;})) {
        error = 1;
      }
    }}
  } by {
    parallelize i;
    vectorize j;
  }

  if(error)
    fprintf(stderr, "Fail\n");
  else
    fprintf(stderr, "Pass\n");

  return error;
}
