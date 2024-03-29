grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production mulTensor
top::Expr ::= l::Expr r::Expr
{
  top.pp = ppConcat([
             l.pp,
             text("*"),
             r.pp
           ]);

  top.tensorExp =
    tensorMul(l.tensorExp, r.tensorExp, top.env);

  propagate controlStmtContext, env;

  local fwrd::Expr = emptyAccess;
  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors,
      fwrd
    );
}
