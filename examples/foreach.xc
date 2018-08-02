#include "tensors.xh"

tensor format mat ({dense, sparse});

indexvar i;

int main() {
  tensor<mat> A = build(tensor<mat>)({1000, 1000});

  // init. values

  foreach(double v : A[2, i]) {
    printf("%3d ", i);
  }
  printf("\n");

  return 0;
}
