grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

concrete production halideTensorExpr_c
top::Stmt_c ::= 
  tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  local iter::Stmt =
    iterateStmt(
      halideTensorExpr(ten.ast, index.ast, value.ast),
      ts.ast
    );

  top.ast = 
    halideSetup(ten.ast, index.ast, value.ast, iter.env)
    (
      iter
    );
}
