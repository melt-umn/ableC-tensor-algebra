grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Add the tensorExp attribute to Expr, and provide a default
   production and a production for parentheses -}
synthesized attribute tensorExp :: TensorExpr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.tensorExp = tensorBaseExpr(top, top.env);
}

aspect production parenExpr
top::Expr ::= e::Expr
{
  top.tensorExp = e.tensorExp;
}

aspect production decExpr
top::Expr ::= e::Decorated Expr
{
  top.tensorExp = e.tensorExp;
}

aspect production transformedExpr
top::Expr ::= original::Expr resolved::Expr
{
  top.tensorExp = resolved.tensorExp;
}

global emptyAccess :: Expr =
  ableC_Expr {
    ({$BaseTypeExpr{extTypeExpr(nilQualifier(), tensorAccType())} _res; 
      memset(&_res, 0, sizeof(_res));
      _res;})
  };
