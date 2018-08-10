#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

tensor format mat ({dense, dense});

int main() {
  tensor<mat> res = inst read<tensor<mat>>("matrix.mtx");

  for(int i = 0; i < dimenof(res)[0]; i++) {
    for(int j = 0; j < dimenof(res)[1]; j++) {
      printf("%4.4f ", res[i, j]);
    }
    printf("\n");
  }

  return 0;
}
