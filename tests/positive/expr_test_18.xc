#include "tensors.xh" 
#include <math.h>

tensor format mat = ({dense, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v - e < -0.0001 || v - e > 0.0001) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {  
  tensor<mat> A = build(tensor<mat>)({4,4});
  tensor<mat> B = build(tensor<mat>)({4,4});
  
  value(B)(0,0) = 4.0;
  value(B)(1,1) = 1.0;
  value(B)(2,3) = 0.25;
  value(B)(3,0) = 9.0;
  
  tensor A(i,j) = sqrt(( B(i,j) )) + ((1)); // this isn't working. The + 1 get's dropped
  // addition is simingly sparse when using constants

  assert_f(value(A)(0,0), 3.0);
  assert_f(value(A)(0,1), 0.0);
  assert_f(value(A)(0,2), 0.0);
  assert_f(value(A)(0,3), 0.0);
  assert_f(value(A)(1,0), 0.0);
  assert_f(value(A)(1,1), 2.0);
  assert_f(value(A)(1,2), 0.0);
  assert_f(value(A)(1,3), 0.0);
  assert_f(value(A)(2,0), 0.0);
  assert_f(value(A)(2,1), 0.0);
  assert_f(value(A)(2,2), 0.0);
  assert_f(value(A)(2,3), 1.5);
  assert_f(value(A)(3,0), 4.0);
  assert_f(value(A)(3,1), 0.0);
  assert_f(value(A)(3,2), 0.0);
  assert_f(value(A)(3,3), 0.0);


  return 0;
}