grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production mulTensor
top::Expr ::= l::Expr r::Expr
{
  propagate substituted;
  top.pp = ppConcat([
             l.pp,
             text("*"),
             r.pp
           ]);

  top.tensorExp =
    tensorMul(top, l.tensorExp, r.tensorExp, top.env, location=top.location);

  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors,
      emptyAccess
    );
}
