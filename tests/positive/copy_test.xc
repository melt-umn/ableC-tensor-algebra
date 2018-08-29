#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});
tensor format tst ({dense, dense});

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
  tensor<mat> org = build(tensor<mat>)({10, 10});
  org[0,0] = 1;
  org[1,2] = 2;
  org[2,0] = 3;
  org[2,2] = 5;
  org[2,8] = 7;
  org[3,5] = 6;
  org[5,9] = 7;

  tensor<mat> t1 = {0};
  t1 = build(tensor<mat>)({10, 10});
  t1[0,0] = 1;
  t1[1,2] = 2;
  t1[2,0] = 3;
  t1[2,2] = 5;
  t1[2,8] = 7;
  t1[3,5] = 6;
  t1[5,9] = 7;

  tensor<mat> t2 = {0};
  t2 = org;
  t2 = t1;

  tensor<tst> t3 = {0};
  t3 = org;

  assert(org[0,0], 1.0); assert(t1[0,0], 1.0); assert(t2[0,0], 1.0); assert(t3[0,0], 1.0);
  assert(org[1,2], 2.0); assert(t1[1,2], 2.0); assert(t2[1,2], 2.0); assert(t3[1,2], 2.0);
  assert(org[2,0], 3.0); assert(t1[2,0], 3.0); assert(t2[2,0], 3.0); assert(t3[2,0], 3.0);
  assert(org[2,2], 5.0); assert(t1[2,2], 5.0); assert(t2[2,2], 5.0); assert(t3[2,2], 5.0);
  assert(org[2,8], 7.0); assert(t1[2,8], 7.0); assert(t2[2,8], 7.0); assert(t3[2,8], 7.0);
  assert(org[3,5], 6.0); assert(t1[3,5], 6.0); assert(t2[3,5], 6.0); assert(t3[3,5], 6.0);
  assert(org[5,9], 7.0); assert(t1[5,9], 7.0); assert(t2[5,9], 7.0); assert(t3[5,9], 7.0);

  freeTensor(org);
  freeTensor(t1);
  freeTensor(t2);
  freeTensor(t3);

  if(error) exit(1);
  return 0;
}
