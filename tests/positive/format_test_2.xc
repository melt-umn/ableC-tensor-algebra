#include "tensors.xh"

tensor format s = ({dense, dense});

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
  tensor<s> A = build (tensor<s>)({3, 3});
  value (A)(0, 1) = 2.0;
  value (A)(2, 0) = 3.0;
  value (A)(2, 2) = 4.0;
  
  value (A)(0,0);
  
  assert_i(A->indices[0][0][0], 3);
  assert_i(A->indices[1][0][0], 3);

  assert_f(A->data[0], 0.0);
  assert_f(A->data[1], 2.0);
  assert_f(A->data[2], 0.0);
  assert_f(A->data[3], 0.0);
  assert_f(A->data[4], 0.0);
  assert_f(A->data[5], 0.0);
  assert_f(A->data[6], 3.0);
  assert_f(A->data[7], 0.0);
  assert_f(A->data[8], 4.0);

  return 0;
}
