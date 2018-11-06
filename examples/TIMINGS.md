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
| LBNL 1   | 34.921s          | 1.801s          | 188 MB             | 259 MB            |
| LBNL 2   | 34.997s          | 3.585s          | 187 MB             | 259 MB            |
| SpMV     | 0.178280s        | 0.258692s       | 23  MB             | 2.6 MB            |
| SDDMM    | 3.262914s        | 1.203078s       | 416 MB             | 94  MB            |
| MTTKRP   | 1.833332s        | 12.724140s      | 25  MB             | 4.2 MB            |

## Computation Timings
| Example | Time with TACO | Time with AbleC |
| ------- | :------------: | :-------------: |
| SpMV    | 0.127142       | 0.085896        |
| SDDMM   | 0.335463       | 1.018344        |
| MTTKRP  | 1.725596       | 12.814500       |

\* Time for the LBNL examples was measured using the Linux *time* command. For other examples timing was performed using the sys/time.h library of C and C++, for TACO and AbleC respectively. The timings were used to surround just the computation in question, though it also included the packing in TACO, as AbleC handles automatic packing for tensor expressions.

† Memory usage was recorded by running the *time* command in verbose mode (-v), and recording maximum resident set size. The numbers may not be entirely accurate, but provide a reasonable estimate for our purposes herein.

We see consistently slow speeds for TACO with the LBNL examples, where there are approximately 1.7 million elements. Testing suggests that this is due to the packing algorithm used in TACO. Combined, the process of TACO reading this file and then packing the tensor in memory seems to take a majority of the execution time.

While we have done our best to implement the TACO versions as best as possible, we do not make any claims about them being as efficient as possible. It is possible there are different ways to solve the problems using TACO that would generate better results, however we have done our best using the limited documentation to implement the algorithms we are interested in exploring.
