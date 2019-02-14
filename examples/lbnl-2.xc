#include "tensors.xh"
#include <stdio.h>

/**
 * This program is very similar to lbnl-1.xc, and commenting there better describes
 * the functioning of the code. 
 * This program differs by looking only at the data sent from one specific 
 * source IP address, instead of all connections on the network.
 */

tensor format src ({dense, sparse, sparse, sparse, sparse}, {0, 2, 1, 3, 4});
tensor format com ({sparse, sparse});

indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  tensor<src> dta = inst read_tensor<tensor<src>>("lbnl-network.tns");

  // Here the result tensor tracks data sent from a specific port to 
  // a destination IP. Since we expect many ports to be unused, this
  // tensor is stored as sparse-sparse, unlike in lbnl-1.xc
  tensor<com> result = 
    build(tensor<com>) ({dimenof(dta)[1], dimenof(dta)[2]});

  // This loop iterates over all elements in the lbnl-network tensor
  // with sender IP address equal to 180 where the value is non-zero
  // This represents iterating over all connections from a certain 
  // IP address (not counting connections over which no data was sent)
  foreach(double v : dta[180, sP, dI, dP, t]) {
    // We are only interested in how much data was sent from each port 
    // to each destination IP, so we ignore the destination port (dP)
    // and time (t)
    double past = result[sP, dI];
    result[sP, dI] = past + v; 
  }

  freeTensor(dta);

  // Same as before, we find the most common connections, but now
  // between ports from IP 180, and destination IPs.
  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});
  foreach(double v : result[snd, dst]) {
    if(v > 500000)
      common[snd, dst] = v;
  }

  freeTensor(result);

  inst write_tensor<tensor<com>>("common.tns", &common);
  
  freeTensor(common);

  return 0;
}
