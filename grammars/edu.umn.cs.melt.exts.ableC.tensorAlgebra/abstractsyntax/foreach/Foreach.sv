grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:foreach;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorForEach
top::Stmt ::= var::Name bounds::Expr body::Stmt
{
  top.functionDefs := [];
  top.pp =
    ppConcat([
      text("foreach ("),
      text("double "),
      var.pp,
      text(" : "),
      bounds.pp,
      text(")\n"),
      body.pp
   ]);

  bounds.env = top.env;

  local tensor :: TensorExpr =
    bounds.tensorExp;
  tensor.fmts = tm:empty(compareString);

  local access :: [Either<Expr String>] =
    tensor.iterAccess;
  
  forwards to
    exprStmt(
      mkStringConst(
        implode(", ",
          map(
            \ e::Either<Expr String> ->
              if e.isLeft
              then show(100, e.fromLeft.pp)
              else e.fromRight
            ,
            access
          )
        ),
        bounds.location
      )
    );
}
