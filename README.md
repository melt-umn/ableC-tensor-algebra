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
```C
tensor format *name* ({*specifiers*} [, {*dimension order*}]);
```
The dimension order part is optional. In this, *name* is the name you wish to give
the tensor format, *specifiers* is a list of the storage specifiers for each
dimension in the tensor, and *dimension order* is an optional integer list
specifying the order in which the dimensions of the tensor should be stored.
Note that if dimension order is used, the values in it must be integre constants,
any other values will not be accepted.

The specifier list is a list of the keywords `sparse` and `dense`, signifying
whether a specific dimension is stored as a sparse or dense dimension.

### Part 2: Building Tensors
Next, we must create a new tensor. In order to do so, we have to declare the 
format of the tensor that we are going to create. It is impossible to change
this, as the format becomes part of the tensor's type. In addition to declaring
the tensor, we wish to setup the tensor, and for this we will use a new
`build` terminal. To create and initialize a tensor we have three options for
how to use `build`:

```C
tensor<*fmt*> <i>name</i> = build(tensor<*fmt*>)({*dimensions*});
tensor<*fmt*> *name* = build(tensor<*fmt*>)([*tensor contents*]);
tensor<*fmt*> *name* = build(tensor<*fmt*>)(*dimensions array*);
```

In all of these, you must provide the type of the tensor to the `build` command,
this type must match that of the variable you are assigning to, otherwise
type errors will occur. Then, we have three options, the first being a curly-bracketed
list of the desired dimensions, if we want a 10 x 6 matrix, for example, we would put 
`{10, 6}`. The second option is to provide the contents of the tensor in an organized
fashion. For example, a small 2x3 matrix with:
```
1 2 3
4 5 6
```
would be declared with `[ [1, 2, 3], [4, 5, 6] ]`. Finally, a tensor can also
be declared using an array with the desired dimensions. 

In general, you will want to use `build` to initialize your tensors. If, for 
some reason you don't want to immediately, assign your tensor the value `{0}`
to ensure the fields of the tensor are properly zeroed.

### Part 3: Tensor Access

### Part 4: Index Variables

### Part 5: Tensor Expression

### Part 6: Foreach Loops

### Part 7: Halide Scheduling

### Part 8: OpenMP Parallelization

### Part 9: Tools
