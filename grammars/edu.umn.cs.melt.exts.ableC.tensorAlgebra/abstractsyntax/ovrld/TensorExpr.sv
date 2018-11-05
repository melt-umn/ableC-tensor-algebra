grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Add the tensorExp attribute to Expr, and provide a default
   production and a production for parentheses -}
synthesized attribute tensorExp :: TensorExpr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.tensorExp = tensorBaseExpr(top, top.env, location=top.location);
}

aspect production parenExpr
top::Expr ::= e::Expr
{
  top.tensorExp = e.tensorExp;
}

global emptyAccess :: Expr =
  ableC_Expr {
    ($BaseTypeExpr{extTypeExpr(nilQualifier(), tensorAccType())}){0}
  };
