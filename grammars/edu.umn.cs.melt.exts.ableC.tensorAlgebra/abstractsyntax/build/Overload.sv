grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

{- This file contains the necessary productions and functions
   to allow the  build(*type*)(*arguments*)  syntax to be overridden
   by other extensions. If not overload for this is provided, it
   will forward to an explicit cast of the first argument to the
   given type.
-}

abstract production build
top::Expr ::= t::TypeName exs::[Expr]
{
  propagate substituted;

  top.pp = 
    ppConcat([
      text("build("),
      t.pp,
      text(")(")]
      ++
      map((.pp), exs)
      ++
      [text(")")]
    );

  local option :: Maybe<Expr> =
    ovrld:applyMaybe3(
      getBuildOverloadProd(t.typerep, top.env),
      t,
      exs,
      top.location
    );

  forwards to
    if option.isJust then option.fromJust
    else
      case t.typerep of
      | tensorType(_, _, _) -> buildTensorExpr(t, exs, location=top.location)
      | _ -> explicitCastExpr(t, head(exs), location=top.location)
      end;
}

function getBuildOverloadProd
Maybe<(Expr ::= TypeName [Expr] Location)> ::= t::Type env::Decorated Env
{
  production attribute overloads :: [Pair<String (Expr ::= TypeName [Expr] Location)>] with ++;
  overloads := [];
  return
    do(bindMaybe, returnMaybe) {
      n :: String <- moduleName(env, t);
      prod :: (Expr ::= TypeName [Expr] Location) <- lookupBy(stringEq, n, overloads);
      return prod;
    };
}
