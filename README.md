# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Move off of parseXXX and move to ableC\_XXX.
* Add check for including tensors.xh header file
* Test OpenMP code generation more
* Extend test-suite to test correctness of code gen
* Add user guide to README
* Find nice examples
* Implement sparse and dense functions over tensor accesses for tensor expressions

## User Guide

### Part 1: Tensor Formats
The first step in declaring and using tensors for this extension is to declare
the necessary tensor formats. This is done with a declaration of the form:
`tensor format *name* ({*specifiers*} [, {*dimension order*}]);`, where the
dimension order part is optional. In this, *name* is the name you wish to give
the tensor format, *specifiers* is a list of the storage specifiers for each
dimension in the tensor, and *dimension order* is an optional integer list
specifying the order in which the dimensions of the tensor should be stored.
Note that if dimension order is used, the values in it must be integre constants,
any other values will not be accepted.

The specifier list is a list of the keywords `sparse` and `dense`, signifying
whether a specific dimension is stored as a sparse or dense dimension.

### Part 2: Tensor Declarations

### Part 3: Tensor Access

### Part 4: Index Variables

### Part 5: Tensor Expression

### Part 6: Foreach Loops

### Part 7: Halide Scheduling

### Part 8: OpenMP Parallelization
