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
  tensor<vec> a = build(tensor<vec>)({10});
  tensor<mat> B = build(tensor<mat>)({10, 8});
  tensor<vec> c = build(tensor<vec>)({8});
  
  value(B)(2, 3) = 0.5;
  value(B)(2, 6) = 1.0;
  value(B)(4, 7) = 1.5;
  value(B)(5, 3) = 2.0;
  value(B)(5, 6) = 2.5;
  value(B)(5, 7) = 3.0;
  
  value(c)(2) = 3.5;
  value(c)(3) = 4.0;
  value(c)(6) = 4.5;
  
  tensor a(i) = B(i,j) * c(j);
  
  assert_f(value(a)(0), 0.0);
  assert_f(value(a)(1), 0.0);
  assert_f(value(a)(2), 6.5);
  assert_f(value(a)(3), 0.0);
  assert_f(value(a)(4), 0.0);
  assert_f(value(a)(5), 19.25);
  assert_f(value(a)(6), 0.0);
  assert_f(value(a)(7), 0.0);
  assert_f(value(a)(8), 0.0);
  assert_f(value(a)(9), 0.0);

  return 0;
}
