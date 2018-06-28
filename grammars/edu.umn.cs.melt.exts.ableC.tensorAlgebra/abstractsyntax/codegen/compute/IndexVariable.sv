grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute before::Maybe<IndexVariable>;
nonterminal IndexVariable with before, proceduralName;

abstract production errorVariable
top::IndexVariable ::=
{
  top.before = nothing();
  top.proceduralName = "error";
}

abstract production variableBottom
top::IndexVariable ::= name::String
{
  top.before = nothing();
  top.proceduralName = name;
}

abstract production variableBefore
top::IndexVariable ::= name::String before::IndexVariable
{
  top.before = just(before);
  top.proceduralName = name;
}

abstract production variable
top::IndexVariable ::= name::String before::Maybe<IndexVariable>
{
  top.before = before;
  top.proceduralName = name;
}

abstract production variableOrder
top::IndexVariable ::= order::[String]
{
  top.before =
    case tail(order) of
    | [] -> nothing()
    | tl -> just(variableOrder(tl))
    end;
  
  top.proceduralName = head(order);
}

function mergeIndex
Maybe<IndexVariable> ::= lst::[IndexVariable]
{
  local lowers::[IndexVariable] =
    flatMap(
      \ var::IndexVariable
      -> case var.before of
         | nothing() -> []
         | just(v) -> [v]
         end
      ,
      lst
    );
  
  local top::[Boolean] =
    map(
      \ var::IndexVariable
      -> !containsBy(
           \ a::IndexVariable
             v::IndexVariable
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
      stringEq(_, _),
      map(
        (.proceduralName),
        filterWithList(lst, top)
      )
    );

  local iv::String = head(topVars);  

  return
    if null(lst)
    then nothing()
    else if null(topVars)
    then just(errorVariable())
    else let nxt::[IndexVariable] =
           flatMap(
             \ v::IndexVariable
             -> if v.proceduralName == iv
                then case v.before of
                     | nothing() -> []
                     | just(x) -> [x]
                     end
                else [v]
             ,
             lst
           )
         in
         let res::Maybe<IndexVariable> =
           mergeIndex(nxt)
         in
         case res of
         | just(errorVariable()) -> just(errorVariable())
         | _ -> just(variable(iv, res))
         end
         end
         end;
}

function contains
Boolean ::= nm::String var::IndexVariable
{
  return var.proceduralName == nm
      || case var.before of
         | nothing() -> false
         | just(x) -> contains(nm, x)
         end;
}

function variableList
[String] ::= var::IndexVariable
{
  return
    var.proceduralName ::
    case var.before of
    | nothing() -> []
    | just(x) -> variableList(x)
    end;
}
