#include "tensors.xh"
#include <stdio.h>

tensor format src ({dense, sparse, dense, sparse, sparse}, {0, 2, 4, 1, 3});
tensor format out ({sparse, sparse});

indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  tensor<src> dta = inst read<tensor<src>>("lbnl-network.tns");
  tensor<out> result = 
    build(tensor<out>)
      ({dimenof(dta)[0] * dimenof(dta)[1],
        dimenof(dta)[2] * dimenof(dta)[3]});

  foreach(double v : dta[sI, sP, dI, dP, t]) {
    result[sI * dimenof(dta)[1] + sP,
          dI * dimenof(dta)[3] + dP] += v;
  }

  freeTensor(dta);

  foreach(double v : result[snd, dst]) {
    printf("Sender: %lu\tDestination: %lu\tAmout: %f\n", snd, dst, v);
  }

  freeTensor(result);

  return 0;
}
