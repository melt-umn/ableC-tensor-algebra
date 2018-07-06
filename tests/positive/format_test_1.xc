#include "tensors.xh"

tensor format s = ({sparse, sparse});

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
  
  assert_i(A->indices[0][0][0], 0);
  assert_i(A->indices[0][0][1], 2);
  assert_i(A->indices[0][1][0], 0);
  assert_i(A->indices[0][1][1], 2);
  assert_i(A->indices[1][0][0], 0);
  assert_i(A->indices[1][0][1], 1);
  assert_i(A->indices[1][0][2], 3);
  assert_i(A->indices[1][1][0], 1);
  assert_i(A->indices[1][1][1], 0);
  assert_i(A->indices[1][1][2], 2);

  assert_f(A->data[0], 2.0);
  assert_f(A->data[1], 3.0);
  assert_f(A->data[2], 4.0);

  return 0;
}
