#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});
tensor format tst ({dense, dense});

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

  printf("%f %f %f %f %f %f %f\n", org[0,0], org[1,2], org[2,0], org[2,2], org[2,8], org[3,5], org[5,9]);
  printf("%f %f %f %f %f %f %f\n", t1[0,0], t1[1,2], t1[2,0], t1[2,2], t1[2,8], t1[3,5], t1[5,9]);
  printf("%f %f %f %f %f %f %f\n", t2[0,0], t2[1,2], t2[2,0], t2[2,2], t2[2,8], t2[3,5], t2[5,9]);
  printf("%f %f %f %f %f %f %f\n", t3[0,0], t3[1,2], t3[2,0], t3[2,2], t3[2,8], t3[3,5], t3[5,9]);

  freeTensor(org);
  freeTensor(t1);
  freeTensor(t2);
  freeTensor(t3);

  return 0;
}
