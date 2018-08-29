#include "tensors.xh"
#include <stdio.h>
#include <locale.h>

// Declare the 3 formats, one for the source tensor, one for all sender-dest pairs
// and one for the much sparser matrix of just the largest connections
tensor format src ({dense, sparse, sparse, sparse, sparse}, {0, 2, 1, 3, 4});
tensor format out ({dense, sparse});
tensor format com ({sparse, sparse});

// Our index variables
indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  setlocale(LC_NUMERIC, "");

  // Read lbnl-network.tns from file and into a new tensor
  tensor<src> dta = inst read_tensor<tensor<src>>("lbnl-network.tns");

  // Declare a tensor with the correct dimensions
  tensor<out> result = 
    build(tensor<out>) ({dimenof(dta)[0], dimenof(dta)[2]});

  // Reduce the tensor by summing over sP, dP, t
  result[sI, dI] = dta[sI, sP, dI, dP, t];

  // Free dta tensor (to hopefully free up memory)
  freeTensor(dta);

  // A tensor of the most common connections
  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});

  // Loop over each non-zero element in the result tensor
  foreach(double v : result[snd, dst]) {
    if(v > 500000) // If the traffic is large enough
      common[snd, dst] = v; // Add it to the common tensor
  }

  freeTensor(result);

  // Write the common tensor out to file
  inst write_tensor<tensor<com>>("common.tns", &common);

  freeTensor(common);

  return 0;
}
