grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:utils;

function makeList
[a] ::= comp::(Integer ::= a a) inc::(a ::= a) start::a i_end::a
{
  return
    if comp(start, i_end) < 0
    then start :: makeList(comp, inc, inc(start), i_end)
    else [];
}

function integerCompare
Integer ::= l::Integer r::Integer
{
  return l - r;
}

function integerEqual
Boolean ::= l::Integer r::Integer
{
  return l == r;
}

function inc
Integer ::= i::Integer
{
  return i+1;
}

function containsAll
Boolean ::= eq::(Boolean ::= a a) itms::[a] lst::[a]
{
  return
    if null(itms)
    then true
    else 
       containsBy(eq, head(itms), lst)
    && containsAll(eq, tail(itms), lst);
}

function getElem
Maybe<a> ::= lst::[a] idx::Integer
{
  return
    if null(lst) || idx < 0
    then nothing()
    else if idx == 0
    then just(head(lst))
    else getElem(tail(lst), idx - 1);
}

function orderList
[a] ::= lst::[a] order::[Integer]
{
  return
    case order of
    | [] -> []
    | h::tl ->
       case getElem(lst, h) of
       | nothing() -> []
       | just(i) -> i :: orderList(lst, tl)
       end
    end;
}
