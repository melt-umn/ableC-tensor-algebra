# Examples
Here we provide a few interesting examples of the things that can be done with this extension. Brief discussions of each file are given below. For each example, we also provide C++ code for TACO as reference for how the program would be implemented in their system. We provide time and memory usage comparisons for the two methods as well.

## LBNL Examples
A series of examples are based on the LBNL Network Tensor taken from FROSTT (The Formidable Reposity of Open Source Sparse Tensors and Tools, <http://frostt.io/>). To run these examples, download the tensor found at <http://frostt.io/tensors/lbnl-network/>, and name it lbnl-network.tns. The tensor is a 5<sup>th</sup> order tensor, with the dimensions, in order, representing: sender IP address, sender port number, destination IP address, destination port number, time. The values in the tensor represent the length of the packets sent from that IP and port combination to another IP and port combination in a specific second.

### Example 1
This first example reads the tensor in from file, and then uses the tensor algebra compilation abilities of the system to fold the dimensions we are not interested in, in this case we chose to only care about the sender and destination's IP addresses. When there are index variables that appear on only one side of the tensor expression, the values are summed together. Mathematically, we interpret a statement like `a[i] = B[i,j]` as:
<dl>a[i] = &Sigma;<sub>i</sub>B[i, j].</dl>
Thus, the statement `result[sI, dI] = dta[sI, sP, dI, dP, t]` sums together all values regardless of sender port, destination port, or time for each sender and destination combination.

With this, we then loop over each value in this new matrix, and if the value is sufficiently large (greater than half a million in our example), we place it into a third matrix, thereby leaving a matrix of only the sender-destination pairs that have the most traffic. This matrix is then output as a .tns file.

This example demonstrates the usefulness of the `foreach` loop. While it is possible to produce similar code using TACO, it is much more complicated, involving using an iterator instead of more intuitive loop syntax.

### Example 2
This example is an extension of the previous one. We work with the same dataset, but this time use the `foreach` loop to select a specific IP number (180) in this case, as we observed they had a lot of connections from the results in example 1.

This is another demonstration of the power of the `foreach` loop, as it allows us to mix index variables and C expressions in describing what parts of the tensor we want to loop over. While we only show an example with one expression, it is possible to have multiple, and they can be in arbitrary places. For example, we could loop through the values for a specific sender IP and destination IP, having sender port, destination port, and time as index variables.

## Performance Statistics
| Example  | Time with TACO | Time with AbleC | Memory\* with TACO | Memory with AbleC |
| -------- | :------------: | :-------------: | :----------------: | :---------------: |
| LBNL 1   | 2m 35.002s     | 3.507s          | 187 MB             | 258 MB            |
| LBNL 2   | ?m ??.???s     | ?.???s          | ??? MB             | ??? MB            |
\* Memory usage was recorded by runing the *time* command in verbose mode (-v), and recording maximum resident set size. The numbers may not be entirely accurate, but provide a reasonable estimate for our purposes herein.

We see consistently slow speeds for TACO with the LBNL examples, where there are approximately 1.7 million elements. Testing suggests that this is due to the packing algorithm used in TACO. Combined, the process of TACO reading this file and then packing the tensor in memory seems to take a majority of the two and a half minutes that the LBNL examples take to run.
