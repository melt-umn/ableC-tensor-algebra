#include "tensors.xh"

tensor format mat ({dense, sparse});

indexvar i, j;

int main() {
  tensor<mat> A = build(tensor<mat>)({1000, 1000});

  // init. values
  A[2, 0] = 3.0;
  A[1, 15] = 1.0;
  A[2,900] = 2.0;
  A[3, 60] = 5.0;
  A[2,100] = 6.0;
  A[15, 2] = 7.0;

  int j = 2;
  foreach(double v : A[j, i]) {
    printf("%3lu ", i);
  }
  printf("\n");

  return 0;
}
