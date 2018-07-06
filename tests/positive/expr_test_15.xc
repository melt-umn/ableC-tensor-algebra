#include "tensors.xh"

tensor format mat = ({dense, sparse});
tensor format ts3 = ({sparse, dense, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf
", asserts, v, e);
    exit(1);
  }
}

int main() {  
  tensor<mat> A = build(tensor<mat>)({3,4});
  tensor<ts3> B = build(tensor<ts3>)({3,3,4});
  tensor<mat> C = build(tensor<mat>)({3,4});
  tensor<mat> D = build(tensor<mat>)({4,4});
  
  value(B)(0,0,0) = 0.5;
  value(B)(0,1,2) = 1.0;
  value(B)(2,0,0) = 1.5;
  value(B)(2,0,2) = 2.0;
  
  value(C)(0,1) = 0.5;
  value(C)(1,1) = 1.0;
  value(C)(1,2) = 1.5;
  
  value(D)(0,1) = 0.5;
  value(D)(0,2) = 1.0;
  value(D)(2,2) = 1.5;
  
  tensor A(i,j) = B(i,k,l) * C(k,j) * D(l,j);
  
  assert_f(value(A)(0,0), 0.0);
  assert_f(value(A)(0,1), 0.125);
  assert_f(value(A)(0,2), 2.25);
  assert_f(value(A)(0,3), 0.0);
  assert_f(value(A)(1,0), 0.0);
  assert_f(value(A)(1,1), 0.0);
  assert_f(value(A)(1,2), 0.0);
  assert_f(value(A)(1,3), 0.0);
  assert_f(value(A)(2,0), 0.0);
  assert_f(value(A)(2,1), 0.375);
  assert_f(value(A)(2,2), 0.0);
  assert_f(value(A)(2,3), 0.0);

  return 0;
}
