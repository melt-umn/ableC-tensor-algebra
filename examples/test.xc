#include "tensors.xh"

tensor format s = ({sparse});

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
}
