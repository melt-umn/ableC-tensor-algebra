grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

concrete productions top::Stmt_c 
| tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  top.ast = 
    halideTensorCompute(ten.ast, index.ast, value.ast, ts.ast);
}
| tnTrns::TensorTransform_t '{'
    ten::PostfixExpr_c '[' index::Expr_c ']' '=' value::Expr_c ';'
  '}' 'by' '{' 'order' 'loops' lst::NameList_c ';' ts::Transformations_c '}'
{
  top.ast =
    halideTensorComputeOrdered(ten.ast, index.ast, value.ast, 
      map((.name), lst.list), ts.ast);
}
| tnTrns::TensorTransform_t '{'
    assign::Identifier_t '=' expr::Expr_c ';'
  '}' 'by' '{' ts::Transformations_c '}'
{
  top.ast =
    halideScalarCompute(fromId(assign), expr.ast, ts.ast);
}
| tnTrns::TensorTransform_t '{'
    assign::Identifier_t '=' expr::Expr_c ';'
  '}' 'by' '{' 'order' 'loops' lst::NameList_c ';' ts::Transformations_c '}'
{
  top.ast =
    halideScalarComputeOrdered(fromId(assign), expr.ast,
      map((.name), lst.list), ts.ast);
}
