#include "tensors.xh"
#include <stdio.h>

tensor format d ({dense});
tensor format s0s1 ({sparse, sparse}, {0, 1});

indexvar i, j;

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
  tensor<d> A = build(tensor<d>)({4});
  tensor<s0s1> B = build(tensor<s0s1>)({4, 4});

  B[0,1] = 0.5;
  B[0,2] = 1.0;
  B[1,0] = 1.5;
  B[1,2] = 2.0;
  B[1,3] = 2.5;
  B[3,0] = 0.5;

  A[i] = B[j,i] * B[j,i];

  assert(A[0], 2.5);
  assert(A[1], 0.25);
  assert(A[2], 5.0);
  assert(A[3], 6.25);

  freeTensor(A);
  freeTensor(B);

  if(error) exit(1);

  return 0;
}
