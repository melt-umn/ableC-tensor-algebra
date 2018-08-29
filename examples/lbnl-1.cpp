#include <iostream>
#include "taco.h"

using namespace taco;

int main(int argc, char* argv[]) {
  // Create formats
  Format src({Dense, Sparse, Sparse, Sparse, Sparse}, {0, 2, 1, 3, 4});
  Format out({Dense, Sparse});
  Format com({Sparse, Sparse});

  // Create tensors
  Tensor<double> dta = read("lbnl-network.tns", src);
  Tensor<double> result ({dta.getDimension(0), dta.getDimension(2)}, out);

  // Form a tensor reduction expression
  IndexVar sI, sP, dI, dP, t;
  result(sI, dI) = dta(sI, sP, dI, dP, t);

  // Compile the expression
  result.compile();

  // Assemble indices and numerically compute the result
  result.assemble();
  result.compute();

  // Create common list
  Tensor<double> common ({result.getDimension(0), result.getDimension(1)}, com);

  auto at = iterate<double>(result);
  auto ait = at.begin();

  while(ait != at.end()) {
    std::vector<long unsigned int> acoord = ait->first;
    std::vector<int> coord;
    for(unsigned int i = 0; i < acoord.size(); i++) {
      coord.push_back(acoord[i]);
    }
    double aval = ait->second;
    if(aval > 500000)
      common.insert(coord, aval);
    ++ait;
  }
  common.pack();

  write("common.tns", common);

  return 0;
}
