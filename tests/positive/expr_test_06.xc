#include "tensors.xh"

tensor format mat = ({dense, dense});
tensor format tns = ({sparse, sparse, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {
  tensor<mat> A = build(tensor<mat>)({5, 5});
  tensor<tns> B = build(tensor<tns>)({5, 12, 14});
  tensor<mat> C = build(tensor<mat>)({12, 5});
  tensor<mat> D = build(tensor<mat>)({14, 5});
  
  value(B)(1,1,2) = 1.0;
  value(B)(1,2,3) = 1.5;
  value(B)(2,2,4) = 2.0;
  value(B)(3,1,4) = 2.5;
  value(B)(3,1,2) = 3.0;
  value(C)(1,3) = 3.5;
  value(C)(2,3) = 4.0;
  value(C)(2,4) = 1.0;
  value(D)(2,3) = 1.5;
  value(D)(3,3) = 2.0;
  value(D)(4,3) = 2.5;
  value(D)(4,4) = 3.0;
  
  tensor A(i,l) = B(i,j,k) * C(j,l) * D(k,l);

  assert_f(value(A)(0,0), 0.0);
  assert_f(value(A)(0,1), 0.0);
  assert_f(value(A)(0,2), 0.0);
  assert_f(value(A)(0,3), 0.0);
  assert_f(value(A)(0,4), 0.0);
  assert_f(value(A)(1,0), 0.0);
  assert_f(value(A)(1,1), 0.0);
  assert_f(value(A)(1,2), 0.0);
  assert_f(value(A)(1,3), 17.25);
  assert_f(value(A)(1,4), 0.0);
  assert_f(value(A)(2,0), 0.0);
  assert_f(value(A)(2,1), 0.0);
  assert_f(value(A)(2,2), 0.0);
  assert_f(value(A)(2,3), 20.0);
  assert_f(value(A)(2,4), 6.0);
  assert_f(value(A)(3,0), 0.0);
  assert_f(value(A)(3,1), 0.0);
  assert_f(value(A)(3,2), 0.0);
  assert_f(value(A)(3,3), 37.625);
  assert_f(value(A)(3,4), 0.0);
  assert_f(value(A)(4,0), 0.0);
  assert_f(value(A)(4,1), 0.0);
  assert_f(value(A)(4,2), 0.0);
  assert_f(value(A)(4,3), 0.0);
  assert_f(value(A)(4,4), 0.0);

  return 0;
}
