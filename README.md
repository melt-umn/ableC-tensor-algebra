# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Fix scalar equations when using halide transform, code is emitted too early.
* Fix error in code-gen causing test 15 to fail. Solution is to move declaration of pA2 to below pA1 computation instead of right before inner loop.
* Extend test-suite to test correctness of code gen
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Add check for including tensors.xh header file
* Find nice examples
* Implement string show using string extension
