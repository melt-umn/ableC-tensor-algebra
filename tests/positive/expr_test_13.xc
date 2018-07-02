#include "tensors.xh"

tensor format mat = ({dense, sparse});
tensor format ts4 = ({sparse, dense, sparse, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {  
  tensor<mat> a = build(tensor<mat>)({4, 5});
  tensor<ts4> B = build(tensor<ts4>)({4, 3, 5, 3});
  tensor<mat> c = build(tensor<mat>)({3, 3});
  
  value(B)(0,0,0,0) = 0.5;
  value(B)(0,1,2,1) = 1.0;
  value(B)(1,2,1,2) = 1.5;
  value(B)(1,1,1,0) = 2.0;
  value(B)(1,2,3,2) = 2.5;
  
  value(c)(0,0) = 0.5;
  value(c)(2,2) = 1.0;
  value(c)(1,0) = 1.5;
  value(c)(2,0) = 2.0;
  
  tensor a(i,k) = B(i,j,k,l) * c(j,l);
  
  assert_f(value(a)(0,0), 0.25);
  assert_f(value(a)(0,1), 0.0);
  assert_f(value(a)(0,2), 0.0);
  assert_f(value(a)(0,3), 0.0);
  assert_f(value(a)(0,4), 0.0);
  assert_f(value(a)(1,0), 0.0);
  assert_f(value(a)(1,1), 4.5); // error
  assert_f(value(a)(1,2), 0.0);
  assert_f(value(a)(1,3), 2.5);
  assert_f(value(a)(1,4), 0.0);
  assert_f(value(a)(2,0), 0.0);
  assert_f(value(a)(2,1), 0.0);
  assert_f(value(a)(2,2), 0.0);
  assert_f(value(a)(2,3), 0.0);
  assert_f(value(a)(2,4), 0.0);
  assert_f(value(a)(3,0), 0.0);
  assert_f(value(a)(3,1), 0.0);
  assert_f(value(a)(3,2), 0.0);
  assert_f(value(a)(3,3), 0.0);
  assert_f(value(a)(3,4), 0.0);

  return 0;
}
