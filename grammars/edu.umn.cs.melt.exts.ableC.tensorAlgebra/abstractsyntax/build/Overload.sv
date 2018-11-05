grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

{- This file contains the necessary productions and attributes
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
  
  forwards to
    case t.typerep.buildProd of
    | just(prod) -> prod(t, exs, top.location)
    | nothing() -> explicitCastExpr(t, head(exs), location=top.location)
    end;
}

synthesized attribute buildProd::Maybe<(Expr ::= TypeName [Expr] Location)> occurs on Type, ExtType;

aspect default production
top::Type ::=
{
  top.buildProd = nothing();
}

aspect production extType
top::Type ::= q::Qualifiers sub::ExtType
{
  top.buildProd = sub.buildProd;
}

aspect default production
top::ExtType ::=
{
  top.buildProd = nothing();
}

aspect production tensorType
top::ExtType ::= fmt::Decorated Name
{
  top.buildProd = just(buildTensorExpr(_, _, location=_));
}