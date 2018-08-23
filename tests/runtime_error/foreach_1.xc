#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

indexvar i;

int main() {
  tensor<mat> matrix = build(tensor<mat>)([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]);
  
  foreach(double v : matrix[3, i]) {
    printf("%f\n", v);
  }

  freeTensor(matrix);

  return 0;
}
