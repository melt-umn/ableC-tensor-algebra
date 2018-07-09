#include "tensors.xh"

tensor format csr = ({dense, sparse});
tensor format csf = ({sparse, sparse, sparse});
tensor format  sv = ({sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf.\n", asserts, v, e);
    exit(1);
  }
}

int main() {
  tensor<csr> A = build(tensor<csr>)({2, 3});
  tensor<csf> B = build(tensor<csf>)({2, 3, 4});
  tensor<sv>  c = build(tensor<sv> )({4});
  
  value(B)(0, 0, 0) = 1.0;
  value(B)(1, 2, 0) = 2.0;
  value(B)(1, 2, 1) = 3.0;
  value(c)(0) = 4.0;
  value(c)(1) = 5.0;
  
  tensor A(i, j) = B(i, j, k) * c(k);
  
  assert_f(value(A)(0,0),  4.0);
  assert_f(value(A)(1,2), 23.0);

  return 0;
}
