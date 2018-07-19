# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Implement tensor assignment to generate compute code
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Fix flow type errors from MWDA analysis
 * TensorAccess: Synthesized equation tensorName exceeds flow type with dependencies on tensorNames
 * TensorAccess: Forward equation exceeds flow type with dependencies on lValue
 * AddTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
 * SubTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
 * MulTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
 * DivTensor: Synthesized equation subed exceeds flow type with dependencies on subNames
