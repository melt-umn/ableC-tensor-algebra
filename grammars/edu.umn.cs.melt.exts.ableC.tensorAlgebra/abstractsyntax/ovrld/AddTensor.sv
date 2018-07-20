grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production addTensor
top::Expr ::= l::Expr r::Expr
{
  propagate substituted;
  top.pp = ppConcat([
             l.pp,
             text("+"),
             r.pp
           ]);

  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors,
      emptyAccess
    );
}
