#include "tensors.xh"
#include <time.h>

tensor format csr({dense, sparse});
tensor format  dv({dense});

indexvar i, j;

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
  tensor<csr> A = build(tensor<csr>)({2000, 2000});
  for(int i = 0; i < dimenof(A)[0]; i++) {
    for(int j = 0; j < dimenof(A)[1]; j++) {
      if(generateProb(0.001)) // sparsity
        A[i,j] = generateRand();
    }
  }

  tensor<dv> x = build(tensor<dv>)({dimenof(A)[1]});
  for(int i = 0; i < dimenof(x)[0]; i++) {
    x[i] = generateRand();
  }

  tensor<dv> z = build(tensor<dv>)({dimenof(A)[0]});
  for(int i = 0; i < dimenof(z)[0]; i++) {
    z[i] = generateRand();
  }

  double alpha = 42.0;
  double beta = 33.0;

  tensor<dv> y = build(tensor<dv>)({dimenof(A)[0]});

  clock_t start, end;

  start = clock();
  for(int c = 0; c < 10000; c++) {
    y[i] = alpha * (A[i,j] * x[j]) + beta * z[i];
  }
  end = clock();

  printf("Runtime: %f\n", ((double)(end - start)) / CLOCKS_PER_SEC);

  inst write_tensor<tensor<dv>>("y.tns", &y);

  freeTensor(A);
  freeTensor(x);
  freeTensor(z);
  freeTensor(y);

  return 0;
}
