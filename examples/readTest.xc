#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

tensor format mat ({dense, dense});

int main() {
  tensor<mat> res = inst read<tensor<mat>>("matrix.mtx");

  unsigned long* dim = dimenof(res);
  for(int i = 0; i < dim[0]; i++) {
    for(int j = 0; j < dim[1]; j++) {
      printf("%4.4f ", res[i, j]);
    }
    printf("\n");
  }

  return 0;
}
