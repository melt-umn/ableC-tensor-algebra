#include <iostream>
#include "taco.h"

using namespace taco;

int main(int argc, char* argv[]) {
  // Create formats
  Format src({Dense, Sparse, Sparse, Sparse, Sparse}, {0, 2, 1, 3, 4});
  Format rds({Dense, Sparse, Sparse}, {0, 2, 1});
  Format out({Dense, Sparse});
  Format com({Sparse, Sparse});

  // Create tensors
  Tensor<double> dta = read("lbnl-network.tns", src);
  Tensor<double> rdc ({dta.getDimension(0), dta.getDimension(1), dta.getDimension(2)}, rds);

  // We must reduce because TACO doesn't have an access
  // As such, we can't add values in the body of the loop,
  // as we did with the foreach loop
  IndexVar sI, sP, dI, dP, t;
  rdc(sI, sP, dI) = dta(sI, sP, dI, dP, t);
  rdc.compile();
  rdc.assemble();
  rdc.compute();

  Tensor<double> result ({rdc.getDimension(1), rdc.getDimension(2)}, out);
  
  // Form a tensor reduction expression
  auto at = iterate<double>(rdc);
  auto ait = at.begin();
  while(ait != at.end()) {
    std::vector<long unsigned int> acoord = ait->first;
    std::vector<int> coord;
    coord.push_back(acoord[1]);
    coord.push_back(acoord[2]);
    if(acoord[0] == 180) {
      double aval = ait->second;
      result.insert(coord, aval);
    }
    ++ait;
  }
  result.pack();

  // Create common list
  Tensor<double> common ({result.getDimension(0), result.getDimension(1)}, com);

  auto bt = iterate<double>(result);
  auto bit = bt.begin();

  while(bit != bt.end()) {
    std::vector<long unsigned int> acoord = bit->first;
    std::vector<int> coord;
    for(unsigned int i = 0; i < acoord.size(); i++) {
      coord.push_back(acoord[i]);
    }
    double aval = bit->second;
    if(aval > 500000)
      common.insert(coord, aval);
    ++bit;
  }
  common.pack();

  write("common.tns", common);

  return 0;
}
