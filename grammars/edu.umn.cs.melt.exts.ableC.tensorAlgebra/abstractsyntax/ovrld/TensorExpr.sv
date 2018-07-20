grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensorExp :: TensorExpr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.tensorExp = tensorBaseExpr(top, top.env, location=top.location);
}

global emptyAccess :: Expr =
  parseExpr(s"""
  ({
    struct tensor_acc x;
    x;
  })
  """);
