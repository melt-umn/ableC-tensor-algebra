grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production subTensor
top::Expr ::= l::Expr r::Expr
{
  top.pp = ppConcat([
             l.pp,
             text("-"),
             r.pp
           ]);

  top.tensorExp =
    tensorSub(l.tensorExp, r.tensorExp, top.env, location=top.location);

  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors,
      emptyAccess
    );
}
