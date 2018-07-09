#include "tensors.xh"

tensor format vec = ({dense});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {
  tensor<vec> a = build (tensor<vec>)({13});
  tensor<vec> b = build (tensor<vec>)({13});
  tensor<vec> c = build (tensor<vec>)({13});
  
  value(b)(2) = 1.0;
  value(b)(6) = 2.0;
  value(b)(11) = 6.0;
  value(c)(2) = 3.0;
  value(c)(7) = 5.0;
  value(c)(11) = -2.0;
  
  tensor a(i) = b(i) + c(i);
  
  assert_f(value(a)( 0), 0.0);
  assert_f(value(a)( 1), 0.0);
  assert_f(value(a)( 2), 4.0);
  assert_f(value(a)( 3), 0.0);
  assert_f(value(a)( 4), 0.0);
  assert_f(value(a)( 5), 0.0);
  assert_f(value(a)( 6), 2.0);
  assert_f(value(a)( 7), 5.0);
  assert_f(value(a)( 8), 0.0);
  assert_f(value(a)( 9), 0.0);
  assert_f(value(a)(10), 0.0);
  assert_f(value(a)(11), 4.0);
  assert_f(value(a)(12), 0.0);

  return 0;
}
