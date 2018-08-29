#include "tensors.xh"

tensor format mat ({dense, sparse});

indexvar i, j;

int asserts = 0;
char error = 0;

void assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0.0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

double data[3] = {3.0, 6.0, 2.0};
int is[3] = {0, 100, 900};

int main() {
  tensor<mat> A = build(tensor<mat>)({1000, 1000});

  // init. values
  A[2, 0] = 3.0;
  A[1, 15] = 1.0;
  A[2,900] = 2.0;
  A[3, 60] = 5.0;
  A[2,100] = 6.0;
  A[15, 2] = 7.0;

  unsigned long j = 2;
  int count = 0;
  foreach(double v : A[j, i]) {
    assert(v, data[count]);
    assert(i, is[count]);
    assert(j, 2);
    count++;
  }

  freeTensor(A);

  if(error) exit(1);
  return 0;
}
