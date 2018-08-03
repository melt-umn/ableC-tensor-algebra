#include "tensors.xh"

tensor format mat ({sparse, sparse});

int main() {
  tensor<mat> matrix = build(tensor<mat>)({100, 100});

  int acc[] = {15, 27};
  matrix[acc] = 3.0;

  printf("matrix[15, 27] = %f\n", matrix[15, 27]);
  printf("matrix[15, 27] = %f\n", matrix[acc]);

  return 0;
}
