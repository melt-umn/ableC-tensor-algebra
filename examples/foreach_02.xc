#include "tensors.xh"

tensor format mat ({sparse, sparse}, {1, 0});

int main() {
  tensor<mat> A = build(tensor<mat>)({1000, 1000});

  // init. values
  A[2, 0] = 3.0;
  A[1, 15] = 1.0;
  A[2,900] = 2.0;

  A[0, 0];

  A[3, 60] = 5.0;
  A[2,100] = 6.0;
  A[15, 2] = 7.0;

  foreach(double v : A) {
    printf("(%3lu, %3lu) = %1.1f\n", index[0], index[1], v);
  }
  printf("\n");

  freeTensor(A);

  return 0;
}
