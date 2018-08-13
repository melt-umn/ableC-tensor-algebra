#define TACO_PARALLEL
#define TACO_THREADS 6

#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

#include <omp.h>

tensor format vec ({dense});
tensor format mat ({dense, dense});

indexvar i, j;

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

  return 0;
}
