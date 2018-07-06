#include "tensors.xh"

tensor format mat = ({dense, sparse});
tensor format ts3 = ({sparse, sparse, sparse});

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
  tensor<ts3> A = build(tensor<ts3>)({2,2,3});
  tensor<ts3> B = build(tensor<ts3>)({2,2,4});
  tensor<mat> C = build(tensor<mat>)({3,4});
  
  value(B)(0,0,0) = 0.5;
  value(B)(0,1,2) = 1.0;
  value(B)(1,1,1) = 1.5;
  value(B)(1,1,3) = 2.0;
  
  value(C)(1,2) = 0.5;
  value(C)(1,3) = 1.0;
  value(C)(2,0) = 1.5;
  
  tensor A(i,j,k) = B(i,j,l) * C(k,l);

  assert_f(value(A)(0,0,0), 0.0);
  assert_f(value(A)(0,0,1), 0.0);
  assert_f(value(A)(0,0,2), 0.75);
  assert_f(value(A)(0,1,0), 0.0);
  assert_f(value(A)(0,1,1), 0.5);
  assert_f(value(A)(0,1,2), 0.0);
  assert_f(value(A)(1,0,0), 0.0);
  assert_f(value(A)(1,0,1), 0.0);
  assert_f(value(A)(1,0,2), 0.0);
  assert_f(value(A)(1,1,0), 0.0);
  assert_f(value(A)(1,1,1), 2.0);
  assert_f(value(A)(1,1,2), 0.0);

  return 0;
}
