grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production assignTensor
top::Expr ::= l::Expr r::Expr
{
  l.lValue = true;
  r.lValue = false;
  
  propagate substituted;
  top.pp = ppConcat([
             l.pp,
             text(" = "),
             r.pp
           ]);

  local lAcc::[String] =
    nubBy(stringEq, flatMap(\l::[String] -> l, l.orders));
  
  local rAcc::[String] =
    nubBy(stringEq, flatMap(\l::[String] -> l, r.orders));
  
  local onlyL::[String] =
    filter(
      \ s::String
      -> !containsBy(stringEq, s, rAcc)
      ,
      lAcc
    );
  
  local lErrors::[Message] =
    if !null(onlyL)
    then [err(top.location, s"Cannot generate code for this calculation since the index variable${if listLength(onlyL) > 1 then "s" else ""} ${implode(", ", onlyL)} occur${if listLength(onlyL) > 1 then "" else "s"} only on the left-hand side.")]
    else [];
  
  forwards to 
    mkErrorCheck(
      lErrors ++ l.errors ++ r.errors,
      mkStringConst(s"lAcc = ${implode(", ", lAcc)}, rAcc = ${implode(", ", rAcc)}", top.location)
    );
}
