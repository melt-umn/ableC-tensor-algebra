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
  tensor<mat> B = build(tensor<mat>)({10, 12});
  tensor<vec> c = build(tensor<vec>)({12});
  
  value(B)(0, 1) = 1.0;
  value(B)(0, 3) = 2.0;
  value(B)(3, 2) = 3.0;
  value(B)(7, 2) = 4.0;
  value(B)(7, 4) = 5.0;
  value(B)(8, 4) = 6.0;
  value(c)(1) = 7.0;
  value(c)(2) = 8.0;
  value(c)(7) = 9.0;

  tensor a(i) = B(i, j) * c(j);

  assert_f(value(a)(0),  7.0);
  assert_f(value(a)(1),  0.0);
  assert_f(value(a)(2),  0.0);
  assert_f(value(a)(3), 24.0);
  assert_f(value(a)(4),  0.0);
  assert_f(value(a)(5),  0.0);
  assert_f(value(a)(6),  0.0);
  assert_f(value(a)(7), 32.0);
  assert_f(value(a)(8),  0.0);
  assert_f(value(a)(9),  0.0);

  return 0;
}
