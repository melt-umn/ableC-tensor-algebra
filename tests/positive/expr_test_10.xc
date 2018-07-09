#include "tensors.xh"

tensor format vec = ({sparse});
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
  tensor<vec> a = build(tensor<vec>)({8});
  tensor<mat> B = build(tensor<mat>)({8, 6});
  tensor<mat> C = build(tensor<mat>)({8, 6});
  tensor<vec> d = build(tensor<vec>)({6});
  
  value(B)(0, 2) = 0.5;
  value(B)(0, 4) = 1.0;
  value(B)(2, 2) = 1.5;
  value(B)(3, 3) = 2.0;
  value(B)(7, 1) = 2.5;
  value(B)(7, 3) = 3.0;
  
  value(C)(0, 2) = 0.5;
  value(C)(0, 4) = 1.0;
  value(C)(3, 3) = 1.5;
  value(C)(7, 3) = 2.0;
  
  value(d)(1) = 0.5;
  value(d)(2) = 1.0;
  value(d)(3) = 1.5;
  
  double alpha = 1.5;
  
  tensor a(i) = ((alpha)) * (B(i,j) + C(i,j)) * d(j);
  
  assert_f(value(a)(0), 1.5);
  assert_f(value(a)(1), 0.0);
  assert_f(value(a)(2), 2.25);
  assert_f(value(a)(3), 7.875);
  assert_f(value(a)(4), 0.0);
  assert_f(value(a)(5), 0.0);
  assert_f(value(a)(6), 0.0);
  assert_f(value(a)(7), 13.125);

  return 0;
}
