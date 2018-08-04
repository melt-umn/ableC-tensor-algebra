grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

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
    ({
      struct tensor_acc x;
      x;
    })
  };
