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
  tensor<mat> A = build(tensor<mat>)({3, 4});
  tensor<mat> B = build(tensor<mat>)({3, 4});
  tensor<mat> C = build(tensor<mat>)({3, 5});
  tensor<mat> D = build(tensor<mat>)({5, 4});
  
  value(B)(0, 0) = 0.5;
  value(B)(1, 2) = 1.0;
  value(B)(2, 3) = 1.5;
  
  value(B)(0, 2) = 0.5;
  value(B)(0, 4) = 1.0;
  value(B)(1, 2) = 1.5;
  value(B)(2, 1) = 2.0;
  
  value(B)(1, 3) = 0.5;
  value(B)(2, 0) = 1.0;
  value(B)(2, 2) = 1.5;
  value(B)(4, 2) = 2.0;
  
  tensor A(i,j) = B(i,j) * ( C(i,k) * D(k,j) );
  
  assert_f(value(A)(0,0), 0.25);
  assert_f(value(A)(0,1), 0.0);
  assert_f(value(A)(0,2), 0.0);
  assert_f(value(A)(0,3), 0.0);
  assert_f(value(A)(1,0), 0.0);
  assert_f(value(A)(1,1), 0.0);
  assert_f(value(A)(1,2), 2.25);
  assert_f(value(A)(1,3), 0.0);
  assert_f(value(A)(2,0), 0.0);
  assert_f(value(A)(2,1), 0.0);
  assert_f(value(A)(2,2), 0.0);
  assert_f(value(A)(2,3), 1.5);

  return 0;
}
