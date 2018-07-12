#include "tensors.xh"

tensor format mat = ({sparse, sparse});
tensor format s = ({sparse});
tensor format scalar = ({});

int main() {
  tensor<s> a = build (tensor<s>)({8});

  tensor<s> b = build (tensor<s>)({8});
  value (b)(0) = 10.0;
  value (b)(2) = 20.0;
  value (b)(3) = 30.0;
  
  tensor a(i) = b(i);
  
  printf("a(%d) = %lf, should be %lf\n", 0, value (a)(0), 10.0);
  printf("a(%d) = %lf, should be %lf\n", 2, value (a)(2), 20.0);
  printf("a(%d) = %lf, should be %lf\n", 3, value (a)(3), 30.0);

  tensor<scalar> x = build (tensor<scalar>)();
  value (x)() = 2.5;
  value (x)() *= 3.0;
  
  printf("x() = %f\n", value (x)());

  tensor<mat> A = build(tensor<mat>)({4, 4});
  value(A)(0,0) = 1.0;
  value(A)(1,1) = 2.0;
  value(A)(1,3) = 0.5;
  value(A)(2,1) = 1.5;
  value(A)(3,2) = 2.5;
  
  tensor<mat> B = build(tensor<mat>)({4, 4});
  value(B)(0,2) = 0.5;
  value(B)(1,1) = 1.5;
  value(B)(1,3) = 2.0;
  value(B)(2,0) = 2.0;
  value(B)(3,2) = 1.0;
  
  tensor x() = A(i,j) * B(i,j);
  printf("x() = %f (should be %f)\n", value(x)(), 6.5);
}
