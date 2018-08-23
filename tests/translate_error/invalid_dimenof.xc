#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

int main() {
  tensor<mat> matrix = build(tensor<mat>)({15, 20});

  int d1 = dimenof(matrix)[0];
  int d2 = dimenof(matrix)[1];
  int d3 = dimenof(matrix)[2];

  freeTensor(matrix);

  return 0;
}
