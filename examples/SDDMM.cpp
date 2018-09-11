#include <iostream>
#include "taco.h"
#include <time.h>

using namespace taco;

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
  Format dcsr({Sparse, Sparse});
  Format   rm({Dense, Dense});
  Format   cm({Dense, Dense}, {1, 0});

  Tensor<double> B ({2000, 2000}, dcsr);
  for(int i = 0; i < B.getDimension(0); i++) {
    if(generateProb(0.001)) {
      for(int j = 0; j < B.getDimension(1); j++) {
        if(generateProb(0.001))
          B.insert({i, j}, generateRand());
      }
    }
  }

  Tensor<double> C ({B.getDimension(0), 3000}, rm);
  for(int i = 0; i < C.getDimension(0); i++) {
    for(int j = 0; j < C.getDimension(1); j++) {
      C.insert({i, j}, generateRand());
    }
  }

  Tensor<double> D ({3000, B.getDimension(1)}, cm);
  for(int i = 0; i < D.getDimension(0); i++) {
    for(int j = 0; j < D.getDimension(1); j++) {
      D.insert({i, j}, generateRand());
    }
  }

  Tensor<double> A ({B.getDimension(0), B.getDimension(1)}, dcsr);
  IndexVar i, j, k;

  clock_t start, end;

  start = clock();
  B.pack();
  C.pack();
  D.pack();
  A(i,j) = B(i,j) * C(i,k) * D(k,j);
  A.compile();
  //for(int c = 0; c < 10000; c++) {
    A.assemble();
    A.compute();
  //}
  end = clock();

  printf("Runtime: %f\n", ((double)(end - start)) / CLOCKS_PER_SEC);

  write("A.mtx", A);
  
  return 0;
}
