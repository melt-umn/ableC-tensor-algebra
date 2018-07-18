grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

global emptyAccess :: Expr =
  parseExpr(s"""
  ({
    struct tensor_acc x;
    x;
  })
  """);

abstract production addTensor
top::Expr ::= l::Expr r::Expr
{
  forwards to emptyAccess;
}
