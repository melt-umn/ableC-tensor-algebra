#include <iostream>
#include "taco.h"
#include <time.h>

using namespace taco;

int getNext();
char generateBool();
double generateRand();
char generateProb(double p);

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

char generateProb(double p) {
  return generateRand() < p;
}

int main(int argc, char* argv[]) {
  Format csr({Dense, Sparse});
  Format  dv({Dense});

  Tensor<double> A ({2000, 2000}, csr);
  for(int i = 0; i < A.getDimension(0); i++) {
    for(int j = 0; j < A.getDimension(1); j++) {
      if(generateProb(0.001))
        A.insert({i, j}, generateRand());
    }
  }

  Tensor<double> x ({A.getDimension(1)}, dv);
  for(int i = 0; i < x.getDimension(0); i++) {
    x.insert({i}, generateRand());
  }

  Tensor<double> z ({A.getDimension(0)}, dv);
  for(int i = 0; i < z.getDimension(0); i++) {
    z.insert({i}, generateRand());
  }

  Tensor<double> alpha(42.0);
  Tensor<double>  beta(33.0);

  Tensor<double> y ({A.getDimension(0)}, dv);
  IndexVar i, j;

  clock_t start, end;

  start = clock();
  A.pack();
  x.pack();
  z.pack();
  y(i) = alpha() * (A(i,j) * x(j)) + beta() * z(i);
  y.compile();
  y.assemble();
  for(int c = 0; c < 100000; c++) {
    y.compute();
  }
  end = clock();

  printf("Runtime: %f\n", ((double)(end - start)) / CLOCKS_PER_SEC);

  write("y.tns", y);
  
  return 0;
}
