#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 4
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
  transform {
    for(unsigned k : P) {
      for(unsigned i : M)
        A[i,k] = (double)rand() / (double)RAND_MAX;
      for(unsigned j : N) 
        B[k,j] = (double)rand() / (double)RAND_MAX;
    }
  } by {
    parallelize k;
  }

  tensor transform {
    C0[i,j] = A[i,k] * B[k,j];
  } by {
    order loops i, j, k;
  }
  
  tensor transform {
    C1[i,j] = A[i,k] * B[k,j];
  } by {}

  C2[i,j] = A[i,k] * B[k,j];

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

  freeTensor(A);
  freeTensor(B);
  freeTensor(C0);
  freeTensor(C1);
  freeTensor(C2);

  if(error) {
    fprintf(stderr, "Match failure\n");
    exit(1);
  }
  
  return error;
}
