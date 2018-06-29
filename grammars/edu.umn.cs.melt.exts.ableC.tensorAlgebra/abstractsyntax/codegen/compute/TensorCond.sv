grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

synthesized attribute setOp :: String;
synthesized attribute tensorElems :: [Either<Name TensorCond>];
synthesized attribute tensorDim :: Integer;
nonterminal TensorCond with setOp, tensorElems, tensorDim;

abstract production nullCond
top::TensorCond ::=
{
  top.setOp = "null";
  top.tensorElems = [];
  top.tensorDim = -1;
}

function isNullCond
Boolean ::= cond::TensorCond
{
  return cond.setOp == "null";
}

abstract production allCond
top::TensorCond ::=
{
  top.setOp = "all";
  top.tensorElems = [];
  top.tensorDim = -1;
}

function isAllCond
Boolean ::= cond::TensorCond
{
  return cond.setOp == "all";
}

abstract production accessCond
top::TensorCond ::= tensor::Name var::Integer
{
  top.setOp = "access";
  top.tensorElems = [left(tensor)];
  top.tensorDim = var;
}

function isAccessCond
Boolean ::= cond::TensorCond
{
  return cond.setOp == "access";
}

abstract production andCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.setOp =
    if isAllCond(l)
    then r.setOp
    else if isAllCond(r)
    then l.setOp
    else if isNullCond(l)
    then r.setOp
    else if isNullCond(r)
    then l.setOp
    else "and";
    
  top.tensorElems = 
    if isAllCond(l)
    then r.tensorElems
    else if isAllCond(r)
    then l.tensorElems
    else if isNullCond(l)
    then r.tensorElems
    else if isNullCond(r)
    then l.tensorElems
    else right(l) :: right(r) :: [];
  
  top.tensorDim =
    if isAllCond(l)
    then r.tensorDim
    else if isAllCond(r)
    then l.tensorDim
    else if isNullCond(l)
    then r.tensorDim
    else if isNullCond(r)
    then l.tensorDim
    else -1;
}

function isAndCond
Boolean ::= cond::TensorCond
{
  return cond.setOp == "and";
}

abstract production orCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.setOp =
    if isNullCond(l)
    then r.setOp
    else if isNullCond(r)
    then l.setOp
    else if isAllCond(l) || isAllCond(r)
    then "all"
    else "or";

  top.tensorElems =
    if isNullCond(l)
    then r.tensorElems
    else if isNullCond(r)
    then l.tensorElems
    else if isAllCond(l) || isAllCond(r)
    then []
    else right(l) :: right(r) :: [];
  
  top.tensorDim =
    if isNullCond(l)
    then r.tensorDim
    else if isNullCond(r)
    then l.tensorDim
    else -1;
}

abstract production orCondOpt
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.setOp =
    if isNullCond(l)
    then r.setOp
    else if isNullCond(r)
    then l.setOp
    else if isAllCond(l) || isAllCond(r)
    then "all"
    else "and";
  
  top.tensorElems =
    if isNullCond(l)
    then r.tensorElems
    else if isNullCond(r)
    then l.tensorElems
    else if isAllCond(l) || isAllCond(r)
    then []
    else right(l) :: right(r) :: [];
  
  top.tensorDim =
    if isNullCond(l)
    then r.tensorDim
    else if isNullCond(r)
    then l.tensorDim
    else -1;
}

function isOrCond
Boolean ::= cond::TensorCond
{
  return cond.setOp == "or";
}

function condEqual
Boolean ::= a::TensorCond b::TensorCond
{
  return 
    a.setOp == b.setOp
    &&
    a.tensorDim == b.tensorDim
    &&
    null(
      removeAllBy(
        \x::Either<Name TensorCond> 
         y::Either<Name TensorCond>
      -> case x, y of
         | left(c), left(d) ->
             c.name == d.name
         | right(c), right(d) ->
             condEqual(c, d)
         | _, _ -> false
         end
        ,
        a.tensorElems,
        b.tensorElems
      )
    );      
}
