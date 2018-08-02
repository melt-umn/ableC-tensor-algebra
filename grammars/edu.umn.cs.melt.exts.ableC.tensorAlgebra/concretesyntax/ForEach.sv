grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- TODO: We should probably have an option for specifying the type of 
   the variable. Currently 'double' is hard coded, as this is the only
   type relavent with tensors, and so that when this is generalized,
   code with it will still function. -}
concrete productions top::Stmt_c
| 'foreach' '(' 'double' var::Identifier_t ':' exp::Expr_c ')' body::Stmt_c
{
  top.ast = tensorForEach(fromId(var), exp.ast, body.ast);
}
