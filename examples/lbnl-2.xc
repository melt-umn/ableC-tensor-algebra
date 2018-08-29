#include "tensors.xh"
#include <stdio.h>
#include <locale.h>

tensor format src ({dense, sparse, sparse, sparse, sparse}, {0, 2, 1, 3, 4});
tensor format out ({dense, sparse});
tensor format com ({sparse, sparse});

indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  setlocale(LC_NUMERIC, "");

  tensor<src> dta = inst read_tensor<tensor<src>>("lbnl-network.tns");
  tensor<com> result = 
    build(tensor<com>) ({dimenof(dta)[1], dimenof(dta)[2]});

  // Loop over all values in dta with sender 180. 
  // We end up with a matrix of sender port and destination IP
  foreach(double v : dta[180, sP, dI, dP, t]) {
    result[sP, dI] += v;
  }

  freeTensor(dta);

  // Same as before, we find the most common connections, but now
  // between ports from IP 180, and destination IPs.
  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});
  foreach(double v : result[snd, dst]) {
    if(v > 500000) // 500,000
      common[snd, dst] = v;
  }

  freeTensor(result);

  inst write_tensor<tensor<com>>("common.tns", &common);
  
  freeTensor(common);

  return 0;
}
