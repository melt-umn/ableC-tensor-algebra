#include "tensors.xh"

tensor format mat = ({dense, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {  
  tensor<mat> A = build(tensor<mat>)({2, 2});
  tensor<mat> B = build(tensor<mat>)({2, 3});
  tensor<mat> C = build(tensor<mat>)({3, 2});
  
  value(B)(0, 0) = 1.0;
  value(B)(0, 1) = 3.0;
  value(B)(1, 2) = 1.0;
  value(C)(0, 0) = 1.0;
  value(C)(1, 0) = 2.0;
  value(C)(1, 1) = 1.0;
  
  tensor A(i,j) = B(i,k) * C(k,j);
  
  assert_f(value(A)(0,0), 7.0);
  assert_f(value(A)(0,1), 3.0);
  assert_f(value(A)(1,0), 0.0);
  assert_f(value(A)(1,1), 0.0);
  
  return 0;
}
