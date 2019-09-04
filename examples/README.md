# Examples
Here we provide a few interesting examples of the things that can be done with this extension. Brief discussions of each file are given below. For each example, we also provide C++ code for TACO as reference for how the program would be implemented in their system. We provide time and memory usage comparisons for the two methods as well.

## LBNL Examples
A series of examples are based on the LBNL Network Tensor taken from FROSTT (The Formidable Repository of Open Source Sparse Tensors and Tools, <http://frostt.io/>). To run these examples, download the tensor found at <http://frostt.io/tensors/lbnl-network/>, and name it lbnl-network.tns. The tensor is a 5<sup>th</sup> order tensor, with the dimensions, in order, representing: sender IP address, sender port number, destination IP address, destination port number, time. The values in the tensor represent the length of the packets sent from that IP and port combination to another IP and port combination in a specific second.

### Example 1
This first example reads the tensor in from file, and then uses the tensor algebra compilation abilities of the system to fold the dimensions we are not interested in, in this case we chose to only care about the sender and destination's IP addresses. When there are index variables that appear on only one side of the tensor expression, the values are summed together. Mathematically, we interpret a statement like `a[i] = B[i,j]` as: a<sub>i</sub> = &Sigma;<sub>i</sub>B<sub>i,j</sub>. Thus, the statement `result[sI, dI] = dta[sI, sP, dI, dP, t]` sums together all values regardless of sender port, destination port, or time for each sender and destination combination.

With this, we then loop over each value in this new matrix, and if the value is sufficiently large (greater than half a million in our example), we place it into a third matrix, thereby leaving a matrix of only the sender-destination pairs that have the most traffic. This matrix is then output as a .tns file.

This example demonstrates the usefulness of the `foreach` loop. While it is possible to produce similar code using TACO, it is much more complicated, involving using an iterator instead of more intuitive loop syntax.

### Example 2
This example is an extension of the previous one. We work with the same dataset, but this time use the `foreach` loop to select a specific IP number (180 in this case), as we observed they had a lot of connections from the results in example 1.

This is another demonstration of the power of the `foreach` loop, as it allows us to mix index variables and C expressions in describing what parts of the tensor we want to loop over. While we only show an example with one expression, it is possible to have multiple, and they can be in arbitrary places. For example, we could loop through the values for a specific sender IP and destination IP, having sender port, destination port, and time as index variables.

It is worth noting that the TACO version of this is implemented very differently, as there is not `foreach` loop with tensors in the manner we permit, instead the only option is to iterate over all values in the tensor. In addition, since TACO does not provide a way to access individual values, we cannot perform the addition in the loop body, and so we must use tensor expressions to fold the input tensor into a 3<sup>rd</sup> order tensor, and then iterate through this, looking for the sender IP to be 180.

## Performance Examples
The LBNL examples above present interesting data analysis uses of TACO and our extension. We now present examples of three tensor expressions, based on examples provided on the TACO website. In these examples we load values into the tensors by pseudo-random means (creating random appearing data, but ensuring consistency between the C and C++ versions). We then compute a specific tensor expression, timing just this expression and the packing of the tensors (as this is automatically done in our extension). These are designed to show the performance of the systems. Details on the examples are linked to the TACO website. On the TACO website, code for these is provided for their system, reading values from tensors and matrices available online. We do not use these, however, as we found in many cases their system (and ours in some cases) took far too long to read the files on our hardware. However, in doing so we reduce the actual compute times to pretty small values, and so run computations 10,000 times to provide reasonably long times for timing using the C and C++ time.h libraries.

### SpMV
<http://tensor-compiler.org/docs/scientific_computing/>

### SDDMM
<http://tensor-compiler.org/docs/machine_learning/>

### MTTKRP
<http://tensor-compiler.org/docs/data_analytics/>

## Performance Statistics
| Example  | Time\* with TACO | Time with AbleC | Memory† with TACO  | Memory with AbleC |
| -------- | :--------------: | :-------------: | :----------------: | :---------------: |
| LBNL 1   | 34.847s          | 1.647s          | 188 MB             | 258 MB            |
| LBNL 2   | 35.398s          | 1.675s          | 188 MB             | 258 MB            |
| SpMV     | 1.275824s        | 0.552290s       | 23  MB             | 2.5 MB            |
| SDDMM    | 3.108825s        | 0.743661s       | 416 MB             | 94  MB            |
| MTTKRP   | 1.742720s        | 1.705486s       | 25  MB             | 4.2 MB            | 

\* Time for the LBNL examples was measured using the Linux *time* command. For following examples timing was performed using the sys/time.h library of C and C++, for TACO and AbleC respectively. The timings were used to surround just the computation in question, though it also included the packing in TACO, as AbleC handles automatic packing for tensor expressions.

† Memory usage was recorded by running the *time* command in verbose mode (-v), and recording maximum resident set size. The numbers may not be entirely accurate, but provide a reasonable estimate for our purposes herein.

We see consistently slow speeds for TACO with the LBNL examples, where there are approximately 1.7 million elements. Testing suggests that this is due to the packing algorithm used in TACO. Combined, the process of TACO reading this file and then packing the tensor in memory seems to take a majority of the two and a half minutes that the first LBNL example takes to run.

While we have done our best to implement the TACO versions as best as possible, we do not make any claims about them being as efficient as possible. It is possible there are different ways to solve the problems using TACO that would generate better results, however we have done our best using the limited documentation to implement the algorithms we are interested in exploring.
