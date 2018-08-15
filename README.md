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
<pre><code>
tensor format <i>name</i> ({<i>specifiers</i>} [, {<i>dimension order</i>}]);
</code></pre>
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

<pre><code>
tensor&lt;<i>fmt</i>&gt; <i>name</i> = build(tensor&lt;<i>fmt</i>&gt;)({<i>dimensions</i>});
tensor&lt;<i>fmt</i>&gt; <i>name</i> = build(tensor&lt;<i>fmt</i>&gt;)([<i>tensor contents</i>]);
tensor&lt;<i>fmt</i>&gt; <i>name</i> = build(tensor&lt;<i>fmt</i>&gt;)(<i>dimensions array</i>);
</code></pre>

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
Accessing and assigning values to tensors is easy, and should be rather
intuitive. It is done using the same syntax as array accesses, though the
one difference is that only a single [ ] is ever used, and indices are 
separated by commas within this. For example, if mat is a matrix, we can
access element (3, 5) by writing:
```C
mat[3, 5]
```
To assign a value to a specific point in a tensor, we can just write an
access with any assignment operator, such as:
```C
mat[3, 5] += 2;
```
We can also use variables or expressions as the indices we wish to access,
so we could write, for example:
```C
for(int i = 0; i < 10; i++) {
  for(int j = 0; j < 10; j++) {
    printf("%f\n", mat[i, j]);
  }
}
```

### Part 4: Index Variables
While sometimes we want variables to represent specific values when accessing
a tensor, as in the above example, there are other times when we want a variable
to represent a series of values, all the possible values that the index could take.
For example, in math, writing `a_i = b_i` doesn't mean that for some specific
value of i, a and b are equal, it means that for all values of i, a and b are equal.
In this case i is what we will call an index variable. To allow this differentiation 
between variables with a value, and index variables, index variables must be declared,
and have their own type `indexvar`. If we want, for example, to declare i, j, k as 
index variables, we can write:
```C
indexvar i, j, k;
```
anywhere in our code.

### Part 5: Tensor Expression
Tensor expressions are the algebra component of any tensor algebra system.
Tensor expressions specify how to take one or more input tensors and convert
their values into values in an output tensor. In writing tensor expressions, 
we write tensor index expressions, where we use index variables to describe 
how we access a tensor. Matrix multiplication is a simple tensor operation, 
common to linear algebra, and can be expressed as:
```C
A[i, j] = B[i, k] * C[k, j];
```
in mathematics we would generally write this as: 
A<sub>i,j</sub> = B<sub>i,k</sub> * C<sub>k,j</sub>.

Our system supports tensor expressions involving addition, subtraction,
multiplication, and division. However, division is not guaranteed to 
function necessarily how traditional division would. If the divisor
of any division would be 0, the system simply does not perform the division,
leaving the value 0, instead of taking a value of infinity, or NaN. In addition to
these arithmetic operators, tensor expressions may contain regular C expressions,
such as in 
```C
a[i] = 4 * b[i];
```

There are a few other limitations to the tensor expressions supported
by this system. First, no index variable may appear only on the left-hand
side. `A[i, j] = b[i];` is not valid, though `a[i] = B[i, j];` is.
In the later case, the columns of B will be summed together into a. 
The other limitation has to do with access patterns, and is more compliated.
In short, code cannot be generated for a tensor expression that must access
a tensor against it's natural access order. For example, if both A and B 
are matrices stored in row-major order, `A[i,j] = B[j,i];` will not be 
valid and a compiler error will be raised. 

### Part 6: Foreach Loops

### Part 7: Halide Scheduling

### Part 8: OpenMP Parallelization

### Part 9: Tools
