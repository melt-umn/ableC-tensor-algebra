#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

int main() {
  tensor<mat> matrix = build(tensor<mat>)({15, 20});

  // Use variables to prevent being made
  // into a compile time check
  int zero = 0, one = 1, two = 2;
  int d1 = dimenof(matrix)[zero];
  int d2 = dimenof(matrix)[one];
  int d3 = dimenof(matrix)[two];

  freeTensor(matrix);

  return 0;
}
