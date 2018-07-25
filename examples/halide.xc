#include <stdio.h>
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
  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N}); 
  tensor<mat> C = build(tensor<mat>)({M, N}); 

  // setup tesnors

  tensor transform {
    A[i,j] = B[i,k] * C[i,j] * D[k,l];
  } by {
    
  }
}
