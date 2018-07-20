grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorAssignToTensor
top::Expr ::= tensor::Expr idx::Expr right::Expr
{
  forwards to
    mkStringConst("Not supported yet", top.location);
}

abstract production tensorAssignToScalar
top::Expr ::= out::Expr ex::Expr
{
  forwards to
    mkStringConst("Not supported yet", top.location);
}
