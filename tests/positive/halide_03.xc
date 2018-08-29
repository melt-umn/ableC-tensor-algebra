#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

#define NUM_THREADS 8
#define TILE_DIM 70
#define UNROLL_SIZE 5
#define VECTOR_SIZE 8

#define M 3
#define N 5
#define P 7

tensor format mat ({dense, dense});

indexvar i, j, k;

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

int main() {
  tensor<mat> A = build(tensor<mat>)({M, P});
  tensor<mat> B = build(tensor<mat>)({P, N});

  double c, d;

  A[0,0] = 1; A[0,1] = 2; A[0,2] = 3; A[0,3] = 4; A[0,4] = 5; A[0,5] = 6; A[0,6] = 7;
  A[1,0] = 2; A[1,1] = 3; A[1,2] = 4; A[1,3] = 5; A[1,4] = 6; A[1,5] = 7; A[1,6] = 0;
  A[2,0] = 3; A[2,1] = 4; A[2,2] = 0; A[2,3] = 8; A[2,4] = 7; A[2,5] = 6; A[2,6] = 5;

  B[0,0] = 1; B[0,1] = 2; B[0,2] = 3; B[0,3] = 4; B[0,4] = 5;
  B[1,0] = 0; B[1,1] = 2; B[1,2] = 1; B[1,3] = 4; B[1,4] = 5;
  B[2,0] = 1; B[2,1] = 2; B[2,2] = 0; B[2,3] = 3; B[2,4] = 6;
  B[3,0] = 1; B[3,1] = 2; B[3,2] = 3; B[3,3] = 4; B[3,4] = 7;
  B[4,0] = 7; B[4,1] = 6; B[4,2] = 5; B[4,3] = 4; B[4,4] = 3;
  B[5,0] = 0; B[5,1] = 1; B[5,2] = 3; B[5,3] = 0; B[5,4] = 2;
  B[6,0] = 4; B[6,1] = 3; B[6,2] = 1; B[6,3] = 8; B[6,4] = 7;

  tensor transform {
    c = A[i,k] * B[k,j];
  } by {}

  d = A[i,k] * B[k, j];

  assert(c, 1411.00);
  assert(d, 1411.00);

  freeTensor(A);
  freeTensor(B);

  if(error) exit(1);
  return 0;
}
