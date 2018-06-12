#include <stdio.h>
#include "tensors.xh"

int main() {
  tensor format csv = format({dense, sparse});
  tensor format cvs = format({dense, sparse}, {1, 0});
  tensor format three = format ({dense, sparse, sparse});
  tensor format d = format ({dense, dense});

  tensor t = build  tensor({3, 4 / 2}, csv);
  tensor s = build tensor({6 + 1, 9}, cvs);
  tensor q = build tensor([[1.0, 3.2, 6.9], [2.3, 6.7, 8.2]], csv);
  tensor w = build tensor({6, 7 * 3, 2}, three);
  tensor z = build tensor([[[1.2, 3.7], [6.2, 5.1]], [[2.1, 3.2], [3.4, 3.1]]], three);

  //float res = tensor z(1, 1, 0);
  //printf("Eventually this will be true %f == 3.4\n", res);

  //printf("This should evaluate to %f: %f\n", 0.0, tensor s(0, 7));
  //tensor value s(0, 7) = 2.2 / 3.2;
  //printf("This should evaluate to %f: %f\n", 2.2 / 3.2, tensor s(0, 7));

////Error testing
//  tensor err0 = build tensor({2, 8}, two); // undeclared format two
//  tensor err1 = build tensor({3, 2}, three); // three has 3 dimensions, only 2 provided
//  tensor err2 = build tensor({1, 9 , 8+2}, csv); // csv has only 2 dimensions, 3 provided
//  tensor err3 = build tensor({7*6}, cvs); // cvs has 2 dimensions, only one provided
//  tensor err4 = build tensor([[2, 1], [3, 2, 3]], cvs); // length of inside dimensions do not match
//  tensor err5 = build tensor([[2, 1], [3.2, 6.7*2.5]], three); // three has 3 dimensions, only 2 provided

  tensor format f;

  f = format({dense, sparse});
  tensor format ft = format({dense, sparse, sparse});
  ft = format({dense, sparse, dense}, {1, 0, 2});

  t = build tensor({3, 9}, f);
  t = build tensor({4, 6, 7}, format({sparse, sparse, dense}, {2, 1, 0}));
  t = build tensor([[3.2, 7.5, 6.2 / 3.1], [1.4, 2.3, 5.3]], f);

  printf("Hello, world!\n");

  return 0;
}
