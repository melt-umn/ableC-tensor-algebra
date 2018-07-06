grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:syntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function defaultOrder
[Integer] ::= order::Integer
{
  return createDefaultOrder(order, 0);
}

function createDefaultOrder
[Integer] ::= order::Integer n::Integer
{
  return
    if n == order
    then []
    else n :: createDefaultOrder(order, n + 1);
}

function checkOrder
Boolean ::= order::[Integer]
{
  return containsAll(defaultOrder(listLength(order)), order);
}

function containsAll
Boolean ::= items::[Integer] lst::[Integer]
{
  return null(items) || (contains(head(items), lst) && containsAll(tail(items), lst));
}

function contains
Boolean ::= s::Integer lst::[Integer]
{
  return (!null(lst)) && (s == head(lst) || contains(s, tail(lst)));
}


function generateSubstitutions
[Substitution] ::= lst::[Expr] index::Integer
{
  return if !null(lst)
         then declRefSubstitution(s"__arr${toString(index)}__", head(lst)) ::
              generateSubstitutions(tail(lst), index + 1)
         else [];
}

function generateArray
String ::= lst::[Expr] index::Integer
{
  return case lst of
         | [] -> ""
         | _::[] -> s"__arr${toString(index)}__"
         | _::tl -> s"__arr${toString(index)}__, ${generateArray(tl, index + 1)}"
         end;
}

function errorCheckTypes
[Message] ::= h::Expr lst::[Expr] env::Decorated Env
{
  h.env = env;
  h.returnType = nothing();
  
  return if !h.typerep.isIntegerType
         then [err(h.location, s"Dimensions of tensor must be integer type (got ${showType(h.typerep)})")]
         else []
         ++
         case h of
         | errorExpr(errs) -> errs
         | _ -> []
         end
         ++
         if !null(lst)
         then errorCheckTypes(head(lst), tail(lst), env)
         else [];
}

function countBy
Integer ::= eq::(Boolean ::= a a) elem::a lst::[a]
{
  return
    if null(lst)
    then 0
    else
      if eq(elem, head(lst))
      then 1 + countBy(eq, elem, tail(lst))
      else countBy(eq, elem, tail(lst));
}

function getBy
Maybe<a> ::= eq::(Boolean ::= a) lst::[a]
{
  return
    if null(lst)
    then nothing()
    else
      if eq(head(lst))
      then just(head(lst))
      else getBy(eq, tail(lst));
}
