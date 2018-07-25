# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Fix error in code-gen causing test 15 to fail. Solution is to move declaration of pA2 to below pA1 computation instead of right before inner loop.
* Fix error where parentheses expressions are not intepreted correctly.
* Extend test-suite to test correctness of code gen (expr\_test\_09 has been implemented)
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Investigate and implement loop parallelization where possible
* Add check for including tensors.xh header file
* Find nice examples
* Implement string show using string extension
