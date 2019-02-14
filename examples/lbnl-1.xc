#include "tensors.xh"
#include <stdio.h>

/**
 * This program demonstrates features of the tensor algebra system using the 
 * lbnl-network tensor. This tensor holds data about network usage, tracking the
 * amount of data sent from a specific address (IP and port) to a specific address
 * at a specific time.
 */

// Declare the 3 formats, one for the source tensor, one for all sender-dest pairs
// and one for the much sparser matrix of just the largest connections

// This format represents a 5th order tensor, with the indices representing (in order)
// sender IP address, sender port number, destination IP address, destination port number
// and time. The storage is sparse in all dimensions other than sender IP, since we
// expect an individual to send data to relatively few other IP addresses, through 
// relatively few ports, relatively infrequently. This is also the reason that it is
// stored in a specified order, stored as: sender IP, destination IP, sender port, 
// destination port, time.
tensor format src ({dense, sparse, sparse, sparse, sparse}, {0, 2, 1, 3, 4});

// This format represents a flattened form of the previous format, in this case we have
// flattened to only track the sender IP and the destination IP. Since we expect every
// sender to have sent data on the network, that dimension is dense. Since we do not 
// expect every sender to send data to every other machine on the network, we store
// the destination IP address layer in a sparse format.
tensor format out ({dense, sparse});

// This format is used to represent the most commonly used connections on the network
// It is sparse since we are only interested in the few most high traffic connections
// so most elements in the tensor are left as 0s.
tensor format com ({sparse, sparse});

// Our index variables
indexvar sI, sP, dI, dP, t;
indexvar snd, dst;

int main() {
  // Read lbnl-network.tns from file and into a new tensor
  // This tensor is of order 5, with the indices representing
  // sender IP, sender port, destination IP, destination port, and time
  tensor<src> dta = inst read_tensor<tensor<src>>("lbnl-network.tns");

  // Declare a tensor to hold the "flattened" result, which will only
  // track the sender IP and destination IP addresses.
  tensor<out> result = 
    build(tensor<out>) ({dimenof(dta)[0], dimenof(dta)[2]});

  // Reduce the lbnl-network tensor. We do this by leaving the 
  // sender port (sP), destination port (dP), and time (t) index vars
  // off of the left hand side. Thus, they are automatically reduced
  // via summation.
  // Thus, the result is that the tensor result holds values representing
  // the total volume of traffic between a sender ip (sI) and a destination
  // IP (dI), regardless of the source/destination port or the time it was
  // sent at.
  result[sI, dI] = dta[sI, sP, dI, dP, t];

  // Free dta tensor (to hopefully free up memory)
  freeTensor(dta);

  // A tensor of the most common connections. Since there are fewer of these
  // we store them in a sparse-sparse format
  tensor<com> common = build(tensor<com>)({dimenof(result)[0], dimenof(result)[1]});

  // Use a foreach loop to iterate over every element in result.
  // Since the last dimension of result (the dimension indexed by dst)
  // is sparse, this loop only iterates over the non-zero elements
  foreach(double v : result[snd, dst]) {
    if(v > 500000) // We see if the amount of traffic is over some threshold
      // If it is, we add the element to the common tensor, at the same index
      // by using v for the value and snd and dst for the indices
      common[snd, dst] = v;
  }

  // Free result tensor
  freeTensor(result);

  // Write the common tensor out to file
  inst write_tensor<tensor<com>>("common.tns", &common);

  // Free common tensor
  freeTensor(common);

  return 0;
}
