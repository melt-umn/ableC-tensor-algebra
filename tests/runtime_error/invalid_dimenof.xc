#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

int main() {
  tensor<mat> matrix = build(tensor<mat>)({15, 20});

  // Use 0 + ... to prevent being made
  // into a compile time check
  int d1 = dimenof(matrix)[0 + 0];
  int d2 = dimenof(matrix)[0 + 1];
  int d3 = dimenof(matrix)[0 + 2];

  freeTensor(matrix);

  return 0;
}
