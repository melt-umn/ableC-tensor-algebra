#include "tensors.xh"
#include <stdio.h>

tensor format mat ({dense, sparse});

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

int main() {
  tensor<mat> matrix = build(tensor<mat>)([[1, 2, 3], [4, 5, 6]]);

  assert(matrix[0,0], 1.0);
  assert(matrix[0,1], 2.0);
  assert(matrix[0,2], 3.0);
  assert(matrix[1,0], 4.0);
  assert(matrix[1,1], 5.0);
  assert(matrix[1,2], 6.0);

  freeTensor(matrix);

  if(error) exit(1);
  return 0;
}
