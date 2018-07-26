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

function zip3
[a] ::= f::(a ::= b c d) lstA::[b] lstB::[c] lstC::[d]
{
  return
    if null(lstA) || null(lstB) || null(lstC)
    then []
    else f(head(lstA), head(lstB), head(lstC))
      :: zip3(f, tail(lstA), tail(lstB), tail(lstC));
}

function zip4
[a] ::= f::(a ::= b c d e) lstA::[b] lstB::[c] lstC::[d] lstD::[e]
{
  return
    if null(lstA) || null(lstB) || null(lstC) || null(lstD)
    then []
    else f(head(lstA), head(lstB), head(lstC), head(lstD))
      :: zip4(f, tail(lstA), tail(lstB), tail(lstC), tail(lstD));
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

function mapTail
[a] ::= f::(a ::= [b]) lst::[b]
{
  return
    if null(lst)
    then []
    else f(tail(lst))
      :: mapTail(f, tail(lst));
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
      containsBy(eq, head(items), array)
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

function zip6
[a] ::= f::(a ::= b c d e f g) l1::[b] l2::[c] l3::[d] l4::[e] l5::[f] l6::[g]
{
  return
    if null(l1) || null(l2) || null(l3) || null(l4) || null(l5) || null(l6)
    then []
    else f(head(l1), head(l2), head(l3), head(l4), head(l5), head(l6))
      :: zip6(f, tail(l1), tail(l2), tail(l3), tail(l4), tail(l5), tail(l6));
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
