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
         | sub(l, r) -> getTensors(l) ++ getTensors(r)
         | mul(l, r) -> getTensors(l) ++ getTensors(r)
         | div(l, r) -> getTensors(l) ++ getTensors(r)
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
  local vars::[IndexVariable] =
    variableOrder(start) ::
    map(variableOrder(_), orders);

  local merge::Maybe<IndexVariable> =
    mergeIndex(vars);
  
  return
    case merge of
    | nothing() -> []
    | just(errorVariable()) -> []
    | just(m) -> variableList(m)
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
         | sub(l, r) -> parseOrder(l, env) ++ parseOrder(r, env)
         | mul(l, r) -> parseOrder(l, env) ++ parseOrder(r, env)
         | div(l, r) -> parseOrder(l, env) ++ parseOrder(r, env)
         end;
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

function filterWithList
[a] ::= lst::[a] bools::[Boolean]
{
  return
    if null(lst)
    then []
    else if head(bools)
         then head(lst) :: filterWithList(tail(lst), tail(bools))
         else filterWithList(tail(lst), tail(bools));
}

function containsFunc
Boolean ::= f::(Boolean ::= a) lst::[a]
{
  return
    if null(lst)
    then false
    else f(head(lst))
      || containsFunc(f, tail(lst));
}
