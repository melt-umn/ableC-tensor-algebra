#include "tensors.xh"

tensor format s = ({sparse});

int asserts = 0;

void assert_i(unsigned long v, unsigned long e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lu, expected %lu.
", asserts, v, e);
    exit(1);
  }
}

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf.
", asserts, v, e);
    exit(1);
  }
}

int main() {
  tensor<s> a = build (tensor<s>)({8});
  
  tensor<s> b = build (tensor<s>)({8});
  value (b)(0) = 10.0;
  value (b)(2) = 20.0;
  value (b)(3) = 30.0;
  
  tensor a(i) = b(i) + b(i);
  
  assert_f(value (a)(0), 20.0);
  assert_f(value (a)(1), 0.0);
  assert_f(value (a)(2), 40.0);
  assert_f(value (a)(3), 60.0);
  assert_f(value (a)(4), 0.0);
  assert_f(value (a)(5), 0.0);
  assert_f(value (a)(6), 0.0);
  assert_f(value (a)(7), 0.0);
  
  return 0;
}
