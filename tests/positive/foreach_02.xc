#include "tensors.xh"

tensor format mat ({sparse, sparse}, {1, 0});

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

double nums[6] = {3.0, 7.0, 1.0, 5.0, 6.0, 2.0};
int index0[6] = {2, 15, 1, 3, 2, 2};
int index1[6] = {0, 2, 15, 60, 100, 900};

int main() {
  tensor<mat> A = build(tensor<mat>)({1000, 1000});

  // init. values
  A[2, 0] = 3.0;
  A[1, 15] = 1.0;
  A[2,900] = 2.0;
  A[3, 60] = 5.0;
  A[2,100] = 6.0;
  A[15, 2] = 7.0;

  int count = 0;
  foreach(double v : A) {
    assert(v, nums[count]);
    assert(index[0], index0[count]);
    assert(index[1], index1[count]);
    count++;
  }

  freeTensor(A);

  if(error) exit(1);
  return 0;
}
