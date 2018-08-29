#include "tensors.xh"
#include <stdio.h>

tensor format mat ({sparse, sparse});

tensor format tns ({sparse, sparse, sparse});

indexvar i, j, k;

int asserts = 0;
char error = 0;

int assert(double v, double e) {
  asserts++;
  double diff = v - e;
  diff = diff < 0.0 ? 0.0 - diff : diff;
  if(diff > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %f, expected %f.\n", asserts, v, e);
    error = 1;
  }
}

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

  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      assert(result[i,j], source[j,i]);
    }
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

  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      for(int k = 0; k < 4; k++) {
        assert(res[i,j,k], src[k,i,j]);
      }
    }
  }

  freeTensor(src);
  freeTensor(res);

  if(error) exit(1);
  return 0;
}
