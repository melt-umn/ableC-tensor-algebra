#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

int main() {
  tensor<mat> matrix = build(tensor<mat>)([[1, 2, 3], [4, 5, 6]]);

  printf("matrix[0, 0] = %f\n", matrix[0, 0]);
  printf("matrix[1, 1] = %f\n", matrix[1, 1]);
}
