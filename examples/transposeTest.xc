#include "tensors.xh"
#include <stdio.h>

tensor format mat ({sparse, sparse});

tensor format tns ({sparse, sparse, sparse});

indexvar i, j, k;

int main() {
  tensor<mat> source = build(tensor<mat>)({4, 4});
  source[0,0] = 1.0;
  source[0,2] = 2.0;
  source[1,3] = 3.0;
  source[2,1] = 4.0;
  source[3,0] = 5.0;
  source[3,3] = 6.0;
  
  tensor<mat> result = build(tensor<mat>)({4, 4});
  result[i, j] = source[j, i];

  printf("Source:\n");
  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      printf("%1.1f ", source[i, j]);
    }
    printf("\n");
  }

  printf("\nResult:\n");
  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      printf("%1.1f ", result[i, j]);
    }
    printf("\n");
  }

  freeTensor(source);
  freeTensor(result);

  printf("\n");

  tensor<tns> src = build(tensor<tns>)({4, 4, 4});
  tensor<tns> res = build(tensor<tns>)({4, 4, 4});

  src[0,1,2] = 1.0;
  src[0,2,1] = 2.0;
  src[2,1,0] = 3.0;
  src[3,2,3] = 4.0;
  src[3,0,0] = 5.0;
  src[1,3,1] = 6.0;

  res[i, j, k] = src[k, i, j];

  printf("(1, 2, 0) = %f\n", res[1,2,0]);
  printf("(2, 1, 0) = %f\n", res[2,1,0]);
  printf("(1, 0, 2) = %f\n", res[1,0,2]);
  printf("(2, 3, 3) = %f\n", res[2,3,3]);
  printf("(0, 0, 3) = %f\n", res[0,0,3]);
  printf("(3, 1, 1) = %f\n", res[3,1,1]);

  freeTensor(src);
  freeTensor(res);

  return 0;
}
