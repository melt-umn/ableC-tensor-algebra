grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

autocopy attribute lValue::Boolean occurs on Expr, Exprs, MaybeExpr;

-------------------------------------------
aspect production eqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  lhs.lValue = true;
  rhs.lValue = false;
}

aspect production mulEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  lhs.lValue = true;
  rhs.lValue = false;
}

aspect production divEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  lhs.lValue = true;
  rhs.lValue = false;
}

aspect production addEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  lhs.lValue = true;
  rhs.lValue = false;
}

aspect production subEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  lhs.lValue = true;
  rhs.lValue = false;
}

--------------------------------------------
aspect function ovrld:getAssignOverload
Expr ::= 
  lhs::Expr rhs::Expr loc::Location 
  env::Decorated Env returnType::Maybe<Type>
  prod::ovrld:BinaryProd
  overlodFn::(Maybe<ovrld:BinaryProd> ::= Type Type Decorated Env)
  maybeBaseOpOverloadFn::Maybe<(Maybe<ovrld:BinaryProd> ::= Type Type Decorated Env)>
  defaultProd::ovrld:BinaryProd
{
  lhs.lValue = true;
  rhs.lValue = false;
}
-------------------------------------------

aspect production justExpr
top::MaybeExpr ::= e::Expr
{
  e.lValue = top.lValue;
}

aspect production consExpr
top::Exprs ::= h::Expr t::Exprs
{
  h.lValue = top.lValue;
  t.lValue = top.lValue;
}

aspect production basicVarDeclStmt
top::Stmt ::= t::Type n::Name init::Expr
{
  init.lValue = false;
}

aspect production exprStmt
top::Stmt ::= d::Expr
{
  d.lValue = false;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  c.lValue = false;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  e.lValue = false;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  e.lValue = false;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  i.lValue = false;
  c.lValue = false;
  s.lValue = false;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  c.lValue = false;
  s.lValue = false;
}

aspect production returnStmt
top::Stmt ::= e::MaybeExpr
{
  e.lValue = false;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  e.lValue = false;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  v.lValue = false;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  l.lValue = false;
  u.lValue = false;
}

aspect production exprInitializer
top::Initializer ::= e::Expr
{
  e.lValue = false;
}
