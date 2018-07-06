#include "tensors.xh"

tensor format vec = ({sparse});
tensor format mat = ({dense, sparse});

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
  tensor<vec> a = build(tensor<vec>)({8});
  tensor<mat> B = build(tensor<mat>)({8, 5});
  tensor<vec> c = build(tensor<vec>)({5});
  tensor<vec> d = build(tensor<vec>)({8});
  
  value(B)(1, 3) = 0.5;
  value(B)(3, 3) = 1.0;
  value(B)(3, 4) = 1.5;
  value(B)(4, 2) = 2.0;
  value(B)(7, 1) = 2.5;
  value(B)(7, 2) = 3.0;
  
  value(c)(2) = 0.5;
  value(c)(3) = 1.0;
  value(c)(4) = 1.5;
  
  value(d)(1) = 0.5;
  value(d)(2) = 1.0;
  
  double alpha = 2.0;
  double beta = 0.5;

  tensor a(i) = ((alpha)) * B(i,j) * c(j) + ((beta)) * d(i);

  assert_f(value(a)(0), 0.0);
  assert_f(value(a)(1), 1.25);
  assert_f(value(a)(2), 0.5);
  assert_f(value(a)(3), 6.5);
  assert_f(value(a)(4), 2.0);
  assert_f(value(a)(5), 0.0);
  assert_f(value(a)(6), 0.0);
  assert_f(value(a)(7), 3.0);

  return 0;
}
