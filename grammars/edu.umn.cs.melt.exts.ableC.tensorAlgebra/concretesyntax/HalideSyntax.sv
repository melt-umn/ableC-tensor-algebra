grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

concrete production halideTensorExpr_c
top::Stmt_c ::= 
  tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  top.ast = 
    seqStmt(
      halideSetup(ten.ast, index.ast, value.ast),
      iterateStmt(
        halideTensorExpr(ten.ast, index.ast, value.ast),
        ts.ast
      )
    );
}
