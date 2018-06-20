#include <stdio.h>
#include "tensors.xh"

tensor format csv = ({dense, sparse});
tensor format cvs = ({sparse, sparse}, {1, 0});
tensor format three = ({dense, sparse, sparse});
tensor format d = ({dense, dense});
tensor format f = ({dense, sparse});
tensor format ft = ({dense, sparse, dense}, {1, 0, 2});

tensor format fB = ({dense, sparse});
tensor format fA = ({dense, dense});

int main() {
  tensor<csv> t = build (tensor<csv>) ({3, 4 / 2});
  tensor<cvs> s = build (tensor<cvs>) ({6 + 1, 9});
  tensor<csv> q = build (tensor<csv>) ([[1.0, 3.2, 6.9], [2.3, 6.7, 8.2]]);
  tensor<three> w = build (tensor<three>) ({6, 7 * 3, 2});
  tensor<three> z = build (tensor<three>) ([[[1.2, 3.7], [6.2, 5.1]], [[2.1, 3.2], [3.4, 3.1]]]);

  float res = value (z)(1, 1, 0);
  printf("Eventually this will be true %f == 3.4\n", res);

  printf("This should evaluate to %f: %f\n", 0.0, value (s)(0, 7));
  value (s)(0, 7) = 2.2 / 3.2;
  printf("This should evaluate to %f: %f\n", 2.2 / 3.2, value (s)(0, 7));

////Error testing
//  tensor<two> err0 = build (tensor<two>) ({2, 8}); // undeclared format two
//  tensor<three> err1 = build (tensor<three>) ({3, 2}); // three has 3 dimensions, only 2 provided
//  tensor<csv> err2 = build (tensor<csv>) ({1, 9 , 8+2}); // csv has only 2 dimensions, 3 provided
//  tensor<cvs> err3 = build (tensor<cvs>) ({7*6}); // cvs has 2 dimensions, only one provided
//  tensor<cvs> err4 = build (tensor<cvs>) ([[2, 1], [3, 2, 3]](; // length of inside dimensions do not match
//  tensor<three> err5 = build (tensor<three>) ([[2, 1], [3.2, 6.7*2.5]]); // three has 3 dimensions, only 2 provided

  tensor<f> tensor_t;
  tensor_t = build (tensor<f>) ({3, 9});
  tensor_t = build (tensor<f>) ([[3.2, 7.5, 6.2 / 3.1], [1.4, 2.3, 5.3]]);

  printf("This should be true: 7.5 == %f\n", value (tensor_t)(0, 1));
  printf("As well as this: %f == %f\n", 5.3, value (tensor_t)(1, 2));

  printf("Hello, world!\n");

//  tensor<cvs> mat = inst read<tensor<cvs>>("matrix.mtx");
//  printf("mat(1, 1) = %f\n", value (mat)(0, 0));
//  printf("mat(2, 2) = %f\n", value (mat)(1, 1));
//  printf("mat(3, 3) = %f\n", value (mat)(2, 2));
//  printf("mat(4, 4) = %f\n", value (mat)(3, 3));
//  printf("mat(5, 5) = %f\n", value (mat)(4, 4));
//  printf("mat(6, 6) = %f\n", value (mat)(5, 5));
//  printf("mat(7, 7) = %f\n", value (mat)(6, 6));
//  printf("mat(8, 8) = %f\n", value (mat)(7, 7));
//  printf("mat(9, 9) = %f\n", value (mat)(8, 8));
//  printf("mat(10, 1) = %f\n", value (mat)(9, 0));

  tensor<fA> A = build (tensor<fA>) ({256, 256});
  tensor<fB> B = build (tensor<fB>) ({256, 256});
  tensor<fB> C = build (tensor<fB>) ({256, 256});

  tensor A(i, j) = B(i, j) + C(i, j);
  
  return 0;
}
