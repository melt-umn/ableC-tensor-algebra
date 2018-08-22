#include "tensors.xh"
#include <stdio.h>

tensor format src ({dense, sparse, sparse, sparse, sparse}, {0, 2, 1, 3, 4});
tensor format out ({dense, sparse});
tensor format com ({sparse, sparse});

indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  tensor<src> dta = inst read<tensor<src>>("lbnl-network.tns");
  tensor<out> result = 
    build(tensor<out>) ({dimenof(dta)[0], dimenof(dta)[2]});

  result[sI, dI] = dta[sI, sP, dI, dP, t];
  //foreach(double v : dta[sI, sP, dI, dP, t]) {
  //  result[sI * dimenof(dta)[1] + sP,
  //        dI * dimenof(dta)[3] + dP] += v;
  //}

  freeTensor(dta);

  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});
  foreach(double v : result[snd, dst]) {
    if(v > 500000)
      common[snd, dst] = v;
  }

  freeTensor(result);

  foreach(double v : common[snd, dst]) {
    fprintf(stderr, "Sender: %4lu\tDestination: %4lu\tAmount: %7.2f\n", snd, dst, v);
  }

  freeTensor(common);

  return 0;
}
