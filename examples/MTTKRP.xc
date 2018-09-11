#include "tensors.xh"
#include <time.h>

tensor format csf({sparse, sparse, sparse});
tensor format  rm({dense, dense});

indexvar i, j, k, l;

// Pseudo-random generator
int r = 113;
int P1 = 7919;
int P2 = 65537;
int M = 102611;

int getNext() {
  int tmp = ((P1 * r) + P2) % M;
  r = tmp;
  return tmp;
}

char generateBool() {
  return getNext() % 2 == 0;
}

double generateRand() {
  return ((double)getNext()) / M;
}

// Generate bool with probability p of true
char generateProb(double p) {
  return generateRand() < p;
}

int main() {
  tensor<csf> B = build(tensor<csf>)({2000, 2000, 2000});
  for(int i = 0; i < dimenof(B)[0]; i++) {
    if(generateProb(0.001)) {
      for(int j = 0; j < dimenof(B)[1]; j++) {
        if(generateProb(0.001)) {
          for(int k = 0; k < dimenof(B)[2]; k++) {
            if(generateProb(0.001)) {
              B[i,j,k] = generateRand();
            }
          }
        }
      }
    }
  }

  tensor<rm> C = build(tensor<rm>)({dimenof(B)[1], 20});
  for(int i = 0; i < dimenof(C)[0]; i++) {
    for(int j = 0; j < dimenof(C)[1]; j++) {
      C[i,j] = generateRand();
    }
  }

  tensor<rm> D = build(tensor<rm>)({dimenof(B)[2], 20});
  for(int i = 0; i < dimenof(D)[0]; i++) {
    for(int j = 0; j < dimenof(D)[1]; j++) {
      D[i,j] = generateRand();
    }
  }

  tensor<rm> A = build(tensor<rm>)({dimenof(B)[0], 20});

  clock_t start, end;

  start = clock();
  for(int c = 0; c < 10000; c++) {
    A[i,j] = B[i,k,l] * D[l,j] * C[k,j];
  }
  end = clock();

  printf("Runtime: %f\n", ((double)(end - start)) / CLOCKS_PER_SEC);

  inst write_tensor<tensor<dcsr>>("A.tns", &A);

  freeTensor(A);
  freeTensor(B);
  freeTensor(C);
  freeTensor(D);

  return 0;
}
