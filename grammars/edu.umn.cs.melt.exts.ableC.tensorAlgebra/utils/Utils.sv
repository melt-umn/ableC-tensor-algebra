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
Ord a => [a] ::= inc::(a ::= a) start::a i_end::a
{
  return
    if compare(start, i_end) < 0
    then start :: makeList(inc, inc(start), i_end)
    else [];
}

function inc
Integer ::= i::Integer
{
  return i+1;
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
Eq a => Boolean ::= items::[a] array::[a]
{
  return
    if null(items)
    then false
    else
      contains(head(items), array)
      || containsAny(tail(items), array);
}

function containsAll
Eq a => Boolean ::= items::[a] array::[a]
{
  return
    if null(items)
    then true
    else
      contains(head(items), array)
      && containsAny(tail(items), array);
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
Eq a => Integer ::= i::a lst::[a]
{
  return
    if null(lst)
    then 0
    else 
      if i == head(lst)
      then 1 + count(i, tail(lst))
      else count(i, tail(lst));
}

function lastIndexOf
Eq a => Integer ::= elems::[a] lst::[a]
{
  return lastIndexOf_helper(elems, lst, -1);
}

function lastIndexOf_helper
Eq a => Integer ::= elems::[a] lst::[a] idx::Integer
{
  return
    if null(elems)
    then idx
    else 
      let i::Integer =
        positionOf(head(elems), lst)
      in
      if i > idx
      then lastIndexOf_helper(tail(elems), lst, i)
      else lastIndexOf_helper(tail(elems), lst, idx)
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
        makeList(inc, 0, listLength(lst))
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
