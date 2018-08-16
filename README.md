# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Future Work
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

### Part 4: Tensor Freeing
Despite not being malloc'd themselves, a variety of the components inside
of tensors are malloc'd, and so to prevent memory leaks, call `freeTensor`
on all tensors used in your program.

### Part 5: Index Variables
While sometimes we want variables to represent specific values when accessing
a tensor, as in the above example, there are other times when we want a variable
to represent a series of values, all the possible values that the index could take.
For example, in math, writing <pre>a<sub>i</sub> = b<sub>i</sub></pre> doesn't mean that for some specific
value of i, a and b are equal, it means that for all values of i, a and b are equal.
In this case i is what we will call an index variable. To allow this differentiation 
between variables with a value, and index variables, index variables must be declared,
and have their own type `indexvar`. If we want, for example, to declare i, j, k as 
index variables, we can write:
```C
indexvar i, j, k;
```
anywhere in our code.

### Part 6: Tensor Expression
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

### Part 7: Foreach Loops
Our extension supports iterating over the values in a tensor with a new
`foreach` loop. In this loop, a tensor to iterate over is given, and
accessed using a mix of index variables and expressions (expressions are
not required, it would be possible, though unecessary to not have any
index variables). Expressions provide limits, for example if i is an 
index variable, `mat[2, i]` would iterate over values in the third row
of the matrix. This construct iterates over the values found in the 
regions (limited by the expressions given). For tensors with the last
dimension being sparse, this means the loop will only iterate over
non-zero elements. In other formats, the exact values iterated over
will vary. The loop guarantees to iterate over any non-zero values in
the valid regions, but may iterate over some zeroes as well, depending
on the tensor's format. The syntax for this, inspired by that in Java, 
is show below:
```C
foreach(double v : mat[2, i]) {
  // do something, can include variables v and i
}
```
Inside the loop, the variable declared to receive the values (v above),
has it's appropriate value, as well as any index variables used in 
the expression. So, if the value at mat[2, 2] = 1.0, inside the foreach
there will be an interation with v = 1.0 and i = 2.

### Part 8: Halide Scheduling
To support performance control, this extension extends the ableC Halide 
extension, granting the programmar control over how the loops are 
scheduled. The details of this can be found in their documentation. 
To use the tensor-halide feature, write the following
```C
tensor transform {
  // Any tensor expression
} by {
  // Appropriate Halide scheduling
}
```
There are limitations to be aware of, however. Importantly, the Halide 
transformation can only be used on tensor expressions involving just
dense tensors. The same restriction on index variables appearing only
on the left hand side applies, but the access pattern restriction can
be lifted, by using a new `order loops` transformation. This can be used
in the following manner:
```C
tensor transform {
  // Any tensor expression (involving the index variables i, j, k)
} by {
  order loops i, j, k;
  // Other Halide Schedules
}
```
This transformation will force the system to generate loops with i
as the outer loop, j as the middle loop, and k as the inner loop,
regardless of the access order of the tensors. It should be noted,
however, that this generally results in detrimental cache performance.

### Part 9: OpenMP Parallelization
Out code generation system itself also supports some, rather limited,
parallel code generation using OpenMP pragmas. The limitations are 
beyond the scope of this guide, but if you wish to enable parallelization
where possible, use the following two `#define` directives before
including the tensors.xh header:
```C
#define TACO_PARALLEL
#define TACO_THREADS 8
```
The later define is optional, allowing you to specify how many
threads to run. If not defined, OpenMP will determine this automatically.

### Part 10: Tools
The tensors.xh header file contains a few tools for working with tensors.
Currently, it includes functions to read .mtx files, which can contain
dense or sparse matrix data in them.
