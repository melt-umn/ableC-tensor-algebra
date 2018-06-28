#include "tensors.xh"

tensor format ts3 = ({sparse, dense, sparse});

int asserts = 0;

void assert_f(double v, double e) {
  asserts++;
  if(v != e) {
    fprintf(stderr, "Assert %d failed. Got %lf, expected %lf\n", asserts, v, e);
    exit(1);
  }
}

int main() {  
  tensor<ts3> A = build(tensor<ts3>)({2, 3, 2});
  tensor<ts3> B = build(tensor<ts3>)({2, 3, 2});
  tensor<ts3> C = build(tensor<ts3>)({2, 3, 2});
  
  value(B)(0,1,1) = 0.5;
  value(B)(1,2,0) = 1.0;
  value(B)(0,2,0) = 1.5;
  
  value(C)(1,1,1) = 0.5;
  value(C)(1,2,0) = 1.0;
  value(C)(0,2,0) = 1.5;
  
  tensor A(i,j,k) = B(i,j,k) + C(i,j,k);

  assert_f(valid(A)(0,0,0), 0.0);
  assert_f(valid(A)(0,0,1), 0.0);
  assert_f(valid(A)(0,1,0), 0.0);
  assert_f(valid(A)(0,1,1), 0.5);
  assert_f(valid(A)(0,2,0), 3.0);
  assert_f(valid(A)(0,2,1), 0.0);
  assert_f(valid(A)(1,0,0), 0.0);
  assert_f(valid(A)(1,0,1), 0.0);
  assert_f(valid(A)(1,1,0), 0.0);
  assert_f(valid(A)(1,1,1), 0.5);
  assert_f(valid(A)(1,2,0), 2.0);
  assert_f(valid(A)(1,2,1), 0.0);

  return 0;
}
