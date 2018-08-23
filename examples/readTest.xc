#include <stdio.h>
#include <stdlib.h>
#include "tensors.xh"

tensor format mat ({dense, dense});
tensor format ts4 ({dense, dense, sparse, sparse});

int main() {
  tensor<mat> res = inst read_tensor<tensor<mat>>("matrix.mtx");

  for(int i = 0; i < dimenof(res)[0]; i++) {
    for(int j = 0; j < dimenof(res)[1]; j++) {
      printf("%4.4f ", res[i, j]);
    }
    printf("\n");
  }

  freeTensor(res);

  tensor<ts4> tns = inst read_tensor<tensor<ts4>>("tensor.tns");

  for(int i = 0; i < dimenof(tns)[0]; i++) {
    for(int j = 0; j < dimenof(tns)[1]; j++) {
      for(int k = 0; k < dimenof(tns)[2]; k++) {
        printf("(%d, %d, %d): ", i, j, k);
        for(int l = 0; l < dimenof(tns)[3]; l++) {
          printf("%1.0f ", tns[i,j,k,l]);
	}
	printf("\n");
      }
    }
  }

  freeTensor(tns);

  return 0;
}
