grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function getFormat
TensorFormatItem ::= n::Name env::Decorated Env
{
  return case lookupValue(n.name, env) of
         | b::[] -> case b.typerep of
                    | pointerType(_,
                        tensorType(_, fmt, _)) ->
                          new (fmt.tensorFormatItem)
                    | _ -> errorTensorFormatItem()
                    end
         | _ -> errorTensorFormatItem()
         end;
}

function getTensors
[Name] ::= expr::TensorExpr
{
  return case expr of
         | access(n, _) -> [n]
         | tExpr(_) -> []
         | add(l, r) -> getTensors(l) ++ getTensors(r)
         | mul(l, r) -> getTensors(l) ++ getTensors(r)
         end;
}

function getFormats
[TensorFormatItem] ::= tensors::[Name] env::Decorated Env
{
  return map(getFormat(_, env), tensors);
}


function findOrder
[String] ::= start::[String] orders::[[String]]
{
  local index::[Integer] =
    map(indexOf(\a::String b::String -> a == b,
                start,
                _),
                head(orders)
       );
  local newOrder::[String] =
    formNewOrder(start, head(orders), index);

  return case orders of
         | [] -> start
         | h::tl ->
             if checkIndexArr(index, -1)
             then findOrder(
                  newOrder,
                  tl)
             else []
         end;
}

function formNewOrder
[String] ::= order::[String] add::[String] found::[Integer]
{
  return reverse(formNewOrderFunc(reverse(order), reverse(add), reverse(found), listLength(add)));
}

function formNewOrderFunc
[String] ::= order::[String] add::[String] found::[Integer] index::Integer
{
  return case found of
         | [] -> order
         | h::t -> if h == -1
                   then head(add) :: formNewOrderFunc(order, tail(add), tail(found), index)
                   else if null(order)
                        then formNewOrderFunc([], tail(add), tail(found), index)
                        else if h == index
                             then head(order) :: 
                                  formNewOrderFunc(tail(order), tail(add), tail(found), index-1)
                             else head(order) :: 
                                  formNewOrderFunc(tail(order), add, found, index-1)
         end;
}

function checkIndexArr
Boolean ::= lst::[Integer] min::Integer
{
  return case lst of
         | [] -> true
         | h::tl -> if h == -1
                    then checkIndexArr(tl, min)
                    else if h > min
                    then checkIndexArr(tl, h)
                    else false
         end;
}

function parseOrder
[[String]] ::= expr::TensorExpr env::Decorated Env
{
  return case expr of
         | access(n, idx) ->
             orderList(idx, getFormat(n, env).dimenOrder) :: []
         | tExpr(_) -> []
         | add(l, r) -> parseOrder(l, env) ++ parseOrder(r, env)
         | mul(l, r) -> parseOrder(l, env) ++ parseOrder(r, env)
         end;
}
