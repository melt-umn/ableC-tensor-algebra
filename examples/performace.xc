#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 5
#define VECTOR_SIZE 8

#define TACO_PARALLEL
#define TACO_THREADS 8

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "tensors.xh"

#define M 700
#define N 1000
#define P 2000

tensor format mat ({dense, dense});

indexvar i, j, k;

int main() {

  static double a[M][P];
  static double b[P][N];
  static double c[M][N];

  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N});
  tensor<mat> C = build(tensor<mat>)({M, N});

  fprintf(stderr, "Filling matrices... ");
 
 fprintf(stderr, "A ");
  for(int i = 0; i < M; i++) {
    for(int j = 0; j < P; j++) {
      double r = ((double)rand()) / RAND_MAX;
      a[i][j] = r;
      A[i, j] = r;
    }
  }
  
  fprintf(stderr, "B ");
  for(int i = 0; i < P; i++) {
    for(int j = 0; j < N; j++) {
      double r = ((double)rand()) / RAND_MAX;
      b[i][j] = r;
      B[i, j] = r;
    }
  }

  fprintf(stderr, "Done\n");

  struct timeval start, end;

  fprintf(stderr, "Performing halide multiplication... ");
  gettimeofday(&start, NULL);
  transform {
    for(unsigned i : M, unsigned j : N) {
      c[i][j] = 0;
      for(unsigned k : P) {
        c[i][j] += a[i][k] * b[k][j];
      }
    }
  } by {
    split i into (unsigned i_outer,
                  unsigned i_inner : (M - 1) / NUM_THREADS + 1);
    parallelize i_outer into (NUM_THREADS) threads;
    tile i_inner, j into (TILE_DIM, TILE_DIM);
    split k into (unsigned k_outer,
                  unsigned k_unroll : UNROLL_SIZE,
		  unsigned k_vector : VECTOR_SIZE);
    unroll k_unroll;
    vectorize k_vector;
  }
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n",
          (double) (end.tv_usec - start.tv_usec) / 1000000 +
	  (double) (end.tv_sec - start.tv_sec));
  
  fprintf(stderr, "Performing TACO multiplication... ");
  gettimeofday(&start, NULL);
  C[i, j] = A[i, k] * B[k, j];
  gettimeofday(&end, NULL);
  fprintf(stderr, "%f seconds\n",
          (double) (end.tv_usec - start.tv_usec) / 1000000 +
	  (double) (end.tv_sec - start.tv_sec));

  fprintf(stderr, "Checking results are equal... ");
  char error = 0;
  for(int i = 0; i < M; i++) {
    for(int j = 0; j < N; j++) {
      if(c[i][j] != C[i, j]) {
        error = 1;
      }
    }
  }

  if(error)
    fprintf(stderr, "fail\n");
  else
    fprintf(stderr, "pass\n");

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);

  return error;
}
