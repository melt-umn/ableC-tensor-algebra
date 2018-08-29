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

  foreach(double v : dta[180, sP, dI, dP, t]) {
    result[sP, dI] += v;
  }

  freeTensor(dta);

  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});
  foreach(double v : result[snd, dst]) {
    if(v > 500000) // 500,000
      common[snd, dst] = v;
  }

  freeTensor(result);

  foreach(double v : common[snd, dst]) {
    fprintf(stderr, "Source Port: %4lu   Destination IP: %4lu   Amount: %'14.2f\n", snd, dst, v);
  }

  freeTensor(common);

  return 0;
}
