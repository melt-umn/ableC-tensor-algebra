# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Implement multiplication overload and division overload
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Fix flow type errors from MWDA analysis
> * SubTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
> * TensorAccess: Synthesized equation tensorName exceeds flow type with dependencies on tensorNames
> * TensorAccess: Forward equation exceeds flow type with dependencies on lValue
> * AddTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
