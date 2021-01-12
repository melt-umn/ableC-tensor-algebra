grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:utils;

function maybeMap
[a] ::= f::(Maybe<a> ::= b) lst::[b]
{
  return
    if null(lst)
    then []
    else let res::Maybe<a> = f(head(lst)) in
      if res.isJust
      then res.fromJust :: maybeMap(f, tail(lst))
      else maybeMap(f, tail(lst))
    end;
}

function maybeMapWithTail
[a] ::= f::(Maybe<a> ::= b [b]) lst::[b]
{
  return
    if null(lst)
    then []
    else let res::Maybe<a> = f(head(lst), tail(lst)) in
      if res.isJust
      then res.fromJust :: maybeMapWithTail(f, tail(lst))
      else maybeMapWithTail(f, tail(lst))
    end;
}

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
       contains(head(itms), lst)
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

function zip3
[a] ::= f::(a ::= b c d) lstA::[b] lstB::[c] lstC::[d]
{
  return
    if null(lstA) || null(lstB) || null(lstC)
    then []
    else f(head(lstA), head(lstB), head(lstC))
      :: zip3(f, tail(lstA), tail(lstB), tail(lstC));
}

function zip5
[a] ::= f::(a ::= b c d e f) lstB::[b] lstC::[c] lstD::[d] lstE::[e] lstF::[f]
{
  return
    if null(lstB) || null(lstC) || null(lstD) || null(lstE) || null(lstF)
    then []
    else f(head(lstB), head(lstC), head(lstD), head(lstE), head(lstF))
      :: zip5(f, tail(lstB), tail(lstC), tail(lstD), tail(lstE), tail(lstF));
}

function positionBy
Integer ::= f::(Boolean ::= a) lst::[a]
{
  return positionBy_helper(f, lst, 0);
}

function positionBy_helper
Integer ::= f::(Boolean ::= a) lst::[a] i::Integer
{
  return
    if null(lst)
    then -1
    else 
      if f(head(lst))
      then i
      else positionBy_helper(f, tail(lst), i+1);
}

function mapWithTail
[a] ::= f::(a ::= b [b]) lst::[b]
{
  return
    if null(lst)
    then []
    else f(head(lst), tail(lst))
      :: mapWithTail(f, tail(lst));
}

function containsAny
Boolean ::= eq::(Boolean ::= a a) items::[a] array::[a]
{
  return
    if null(items)
    then false
    else
      contains(head(items), array)
      || containsAny(eq, tail(items), array);
}

function filterWith
[a] ::= lst::[a] inc::[Boolean]
{
  return
    if null(lst) || null(inc)
    then []
    else
      if head(inc)
      then head(lst) :: filterWith(tail(lst), tail(inc))
      else filterWith(tail(lst), tail(inc));
}

function containsWith
Boolean ::= f::(Boolean ::= a) lst::[a]
{
  return
    if null(lst)
    then false
    else
      f(head(lst)) ||
      containsWith(f, tail(lst));
}

function count
Integer ::= f::(Boolean ::= a a) i::a lst::[a]
{
  return
    if null(lst)
    then 0
    else 
      if f(i, head(lst))
      then 1 + count(f, i, tail(lst))
      else count(f, i, tail(lst));
}

function lastIndexOf
Integer ::= f::(Boolean ::= a a) elems::[a] lst::[a]
{
  return lastIndexOf_helper(f, elems, lst, -1);
}

function lastIndexOf_helper
Integer ::= f::(Boolean ::= a a) elems::[a] lst::[a] idx::Integer
{
  return
    if null(elems)
    then idx
    else 
      let i::Integer =
        positionOf(f, head(elems), lst)
      in
      if i > idx
      then lastIndexOf_helper(f, tail(elems), lst, i)
      else lastIndexOf_helper(f, tail(elems), lst, idx)
      end;
}

function filterHead
[a] ::= f::(Boolean ::= a [a]) lst::[a]
{
  return 
    filterHead_helper(
      f, lst,
      map(
        \ i::Integer ->
          take(i, lst)
        ,
        makeList(integerCompare, inc, 0, listLength(lst))
      )
    );
}

function filterHead_helper
[a] ::= f::(Boolean ::= a [a]) lst::[a] hs::[[a]]
{
  return
    if null(lst) || null(hs)
    then []
    else 
      if f(head(lst), head(hs))
      then 
        head(lst) ::
        filterHead_helper(f, tail(lst), tail(hs))
      else
        filterHead_helper(f, tail(lst), tail(hs));
}

function listEqual
Boolean ::= elemEq::(Boolean ::= a b) lst1::[a] lst2::[b]
{
  return
    if null(lst1) && null(lst2)
    then true
    else if null(lst1) || null(lst2)
    then false
    else
      elemEq(head(lst1), head(lst2)) &&
      listEqual(elemEq, tail(lst1), tail(lst2));
}
