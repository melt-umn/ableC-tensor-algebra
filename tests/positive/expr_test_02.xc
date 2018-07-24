#include "tensors.xh"

tensor format csr ({dense, sparse});
tensor format csf ({sparse, sparse, sparse});
tensor format sv  ({sparse});

indexvar i, j, k;

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
  tensor<csr> A = build(tensor<csr>)({2, 3});
  tensor<csf> B = build(tensor<csf>)({2, 3, 4});
  tensor<sv>  c = build(tensor<sv> )({4});

  B[0, 0, 0] = 1.0;
  B[1, 2, 0] = 2.0;
  B[1, 2, 1] = 3.0;

  c[0] = 4.0;
  c[1] = 5.0;

  A[i, j] = B[i, j, k] * c[k];

  assert(A[0, 0], 4.0);
  assert(A[1, 2], 23.0);

  if(error) exit(1);
  return 0;
}
