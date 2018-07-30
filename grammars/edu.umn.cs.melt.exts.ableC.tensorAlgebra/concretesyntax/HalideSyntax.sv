grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

concrete productions top::Stmt_c 
| tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  local iter::Stmt =
    iterateStmt(
      halideTensorExpr(ten.ast, index.ast, value.ast),
      ts.ast
    );

  top.ast =
    halideSetup(ten.ast, index.ast, value.ast, iter);
}
| tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' 'order' 'loops' lst::NameList_c ';' ts::Transformations_c '}'
{
  local iter::Stmt =
    iterateStmt(
      halideTensorExprOrder(ten.ast, index.ast, value.ast, map((.name), lst.list)),
      ts.ast
    );

  top.ast =
    halideSetup(ten.ast, index.ast, value.ast, iter);
}
| tnTrns::TensorTransform_t '{'
    assign::Identifier_t '=' expr::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  local iter::Stmt =
    iterateStmt(
      halideScalarTensorExpr(fromId(assign), expr.ast),
      ts.ast
    );

  top.ast =
    halideScalarSetup(fromId(assign), expr.ast, iter);
}
| tnTrns::TensorTransform_t '{'
    assign::Identifier_t '=' expr::Expr_c ';'
  '}' 'by' '{' 'order' 'loops' lst::NameList_c ';' ts::Transformations_c '}'
{
  local iter::Stmt =
    iterateStmt(
      halideScalarExprOrder(fromId(assign), expr.ast, map((.name), lst.list)),
      ts.ast
    );

  top.ast =
    halideScalarSetup(fromId(assign), expr.ast, iter);
}
