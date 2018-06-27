grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute before::[IndexVariable];
nonterminal IndexVariable with before, proceduralName;

abstract production errorVariable
top::IndexVariable ::=
{
  top.before = [];
  top.proceduralName = "error";
}

abstract production variable
top::IndexVariable ::= name::String before::[IndexVariable]
{
  top.before = before;
  top.proceduralName = name;
}

abstract production variableOrder
top::IndexVariable ::= order::[String]
{
  top.before =
    case tail(order) of
    | [] -> [errorVariable()]
    | tl -> [variableOrder(tl)]
    end;
  
  top.proceduralName = head(order);
}

function mergeIndex
IndexVariable ::= lst::[IndexVariable]
{
  local lowers::[IndexVariable] =
    flatMap(
      \ var::IndexVariable
      -> var.before
      ,
      lst
    );
  
  local top::[Boolean] =
    map(
      \ var::IndexVariable
      -> !containsBy(
           \ a::TensorVariable
             v::TensorVariable
           -> contains(a.proceduralName, v)
           ,
           var,
           lowers
         )
      ,
      lst
    );
  
  local topVars::[String] =
    nubBy(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      map(
        (.proceduralName),
        filterWithList(lst, top)
      )
    );
  
  return
    if null(topVars)
    then errorVariable()
    else let nxt::[IndexVariable] =
           flatMap(
             \ v::IndexVariable
             -> if v.procName == head(topVars)
                then v.before
                else [v]
             ,
             lst
           )
         in
         if null(nxt)
         then variable(head(topVars), [])
         else
           variable(
             head(topVars),
             [mergeIndex(nxt)]
           )
         end;
}

function contains
Boolean ::= nm::String var::IndexVariable
{
  return var.proceduralName == nm
      || foldl(
           \ b1::Boolean b2::Boolean
           -> b1 || b2
           ,
           false,
           map(contains(nm, _), var.before)
         );
}

function getAbove
IndexVariable ::= nm::String var::IndexVariable
{
  return
    if var.proceduralName == nm
    then variable(nm, [])
    else variable(var.proceduralName, 
           map(getAbove(nm, _), var.before));
}

function getBelow
[IndexVariable] ::= nm::String var::IndexVariable
{
  return
    if var.proceduralName == nm
    then var.before
    else foldl(
           \ lst::[IndexVariable]
             var::IndexVariable
           -> lst ++ getBelow(nm, var)
           ,
           [],
           var.before
         );
}

function makeFrom
IndexVariable ::= var::IndexVariable under::[IndexVariable]
{
  return
    if null(var.before)
    then variable(var.proceduralName, under)
    else variable(var.proceduralName, 
           map(makeFrom(_, under), var.before));
}
