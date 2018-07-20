# ableC tensor algebra compiler
A tensor algebra compiler for ableC based on the paper *The Tensor Algebra Compiler* by Fredrik Kjolstad, et. al.

## Todo
* Fix MWDA errors.
  * TensorExpr.sv:921:6: warning: Access of syn attribute conds on ex requires missing inherited attributes edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder, edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:tensorNames to be supplied
  * TensorExpr.sv:921:16: warning: Access of syn attribute sparse on ex requires missing inherited attributes edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder, edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:tensorNames to be supplied
  * TensorExpr.sv:921:27: warning: Access of syn attribute dense on ex requires missing inherited attributes edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder, edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:tensorNames to be supplied
  * TensorExpr.sv:922:6: warning: Access of syn attribute canSub on ex requires missing inherited attributes edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder to be supplied
  * TensorExpr.sv:922:17: warning: Access of syn attribute subed on ex requires missing inherited attributes edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder, edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:subNames to be supplied
  * TensorExpr.sv:919:2: warning: Equation has transitive dependency on child ex's inherited attribute for edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:accessOrder but this equation appears to be missing.
  * TensorExpr.sv:919:2: warning: Equation has transitive dependency on child ex's inherited attribute for edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:subNames but this equation appears to be missing.
  * TensorExpr.sv:919:2: warning: Equation has transitive dependency on child ex's inherited attribute for edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr:tensorNames but this equation appears to be missing.
  * TensorExprAssign.sv:5:0: warning: Implicit forward copy equation for attribute edu:umn:cs:melt:ableC:abstractsyntax:substitution:substituted in production edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld:tensorAssignToTensor exceeds flow type because the forward depends on edu:umn:cs:melt:ableC:abstractsyntax:env:env, edu:umn:cs:melt:ableC:abstractsyntax:host:returnType
  * TensorExprAssign.sv:5:0: warning: Implicit forward copy equation for attribute silver:langutil:pp in production edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld:tensorAssignToTensor exceeds flow type because the forward depends on edu:umn:cs:melt:ableC:abstractsyntax:env:env, edu:umn:cs:melt:ableC:abstractsyntax:host:returnType
  * TensorExprAssign.sv:85:0: warning: Implicit forward copy equation for attribute edu:umn:cs:melt:ableC:abstractsyntax:substitution:substituted in production edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld:tensorAssignToScalar exceeds flow type because the forward depends on edu:umn:cs:melt:ableC:abstractsyntax:env:env, edu:umn:cs:melt:ableC:abstractsyntax:host:returnType
  * TensorExprAssign.sv:85:0: warning: Implicit forward copy equation for attribute silver:langutil:pp in production edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld:tensorAssignToScalar exceeds flow type because the forward depends on edu:umn:cs:melt:ableC:abstractsyntax:env:env, edu:umn:cs:melt:ableC:abstractsyntax:host:returnType
* Find way to handle proper generation of merge lattice, with sub points in the lattice.
* Implement tensor assignment to generate compute code
* Implement sparse and dense functions over tensor accesses for tensor expressions
* Investigate and implement loop parallelization where possible
* Add check for including tensors.xh header file
* Find nice examples

## Ideas
* Have Expr create a TensorExpr which has the needed attributes on it
