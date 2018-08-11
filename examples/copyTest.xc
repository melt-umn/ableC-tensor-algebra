#include <stdio.h>
#include "tensors.xh"

tensor format mat ({dense, sparse});

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

  printf("%f %f %f %f %f %f %f\n", org[0,0], org[1,2], org[2,0], org[2,2], org[2,8], org[3,5], org[5,9]);
  printf("%f %f %f %f %f %f %f\n", t1[0,0], t1[1,2], t1[2,0], t1[2,2], t1[2,8], t1[3,5], t1[5,9]);
  printf("%f %f %f %f %f %f %f\n", t2[0,0], t2[1,2], t2[2,0], t2[2,2], t2[2,8], t2[3,5], t2[5,9]);

  free(org.dims);
  free(org.data);
  free(org.indices[0]);
  free(org.indices[1][0]);
  free(org.indices[1][1]);
  free(org.indices[1]);
  free(org.indices);
  free(t1.dims);
  free(t1.data);
  free(t1.indices[0]);
  free(t1.indices[1][0]);
  free(t1.indices[1][1]);
  free(t1.indices[1]);
  free(t1.indices);
  free(t2.dims);
  free(t2.data);
  free(t2.indices[0]);
  free(t2.indices[1][0]);
  free(t2.indices[1][1]);
  free(t2.indices[1]);
  free(t2.indices);

  return 0;
}
