#include "tensors.xh"

tensor format mat ({sparse, sparse});

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
  tensor<mat> matrix = build(tensor<mat>)({100, 100});

  int acc[] = {15, 27};
  matrix[acc] = 3.0;
  matrix[27, 15] = 6.0;
  
  assert(matrix[15, 27], 3.0);
  assert(matrix[acc], 3.0);

  freeTensor(matrix);

  if(error) exit(1);
  return 0;
}
