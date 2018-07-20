# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Add TensorExpr nonterminal and an attribute of this type on Expr
* Re-implement TensorExpr with add, sub, mul, div, expr
* Implement tensor assignment to generate compute code
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Investigate and implement loop parallelization where possible
* Add check for including tensors.xh header file
* Find nice examples

## Ideas
* Have Expr create a TensorExpr which has the needed attributes on it
