grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

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
    ovrld:applyMaybe2(
      getBuildOverloadProd(t.typerep, top.env),
      exs,
      top.location
    );

  forwards to
    if option.isJust then option.fromJust
    else buildTensorExpr(t, exs, location=top.location);
}

function getBuildOverloadProd
Maybe<(Expr ::= [Expr] Location)> ::= t::Type env::Decorated Env
{
  production attribute overloads :: [Pair<String (Expr ::= [Expr] Location)>] with ++;
  overloads := [];
  return
    do(bindMaybe, returnMaybe) {
      n :: String <- moduleName(env, t);
      prod :: (Expr ::= [Expr] Location) <- lookupBy(stringEq, n, overloads);
      return prod;
    };
}
