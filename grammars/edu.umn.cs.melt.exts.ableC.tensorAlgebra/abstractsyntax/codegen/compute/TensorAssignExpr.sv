grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensorAssign :: TensorExpr;
synthesized attribute tensorValue :: TensorExpr;
synthesized attribute tensorFormat :: tm:Map<Name TensorFormatItem>;
nonterminal TensorAssignExpr with tensorAssign, tensorValue, tensorFormat, location;

abstract production assignExpr
top::TensorAssignExpr ::= base::Name acc::[String] expr::TensorExpr tensors::[Name] formats::[TensorFormatItem]
{
  top.tensorAssign = access(base, acc, location=top.location);
  top.tensorValue = expr;
  top.tensorFormat = tm:add(zipWith(pair(_, _), tensors, formats), tm:empty(\n::Name nm::Name -> compareString(n.name, nm.name)));
}

abstract production assignExprExpr
top::TensorAssignExpr ::= assign::TensorExpr expr::TensorExpr fmt::tm:Map<Name TensorFormatItem>
{
  top.tensorAssign = assign;
  top.tensorValue = expr;
  top.tensorFormat = fmt;
}

abstract production nullAssignExpr
top::TensorAssignExpr ::=
{
  top.tensorAssign = nullTensorExpr(location=top.location);
  top.tensorValue = nullTensorExpr(location=top.location);
  top.tensorFormat = tm:empty(\n::Name nm::Name -> compareString(n.name, nm.name));
}

function findDenseDimension
Pair<String Integer> ::= expr::TensorAssignExpr var::String
{
  return 
    case expr of
    | nullAssignExpr() -> pair("error", -1)
    | _ ->
        let fmt::tm:Map<Name TensorFormatItem> = expr.tensorFormat
        in
        case findDenseDimensionExpr(expr.tensorAssign, var, fmt) of
        | nothing() -> 
            case findDenseDimensionExpr(expr.tensorValue, var, fmt) of
            | nothing() -> pair("error", -1)
            | just(x) -> x
            end
        | just(x) -> x
        end
        end
    end;
}

function findDenseDimensionExpr
Maybe<Pair<String Integer>> ::= expr::TensorExpr var::String fmt::tm:Map<Name TensorFormatItem>
{
  return 
    case expr of
    | access(nm, acc) ->
        let i::Integer =
          positionOf(
            stringEq(_, _),
            var,
            acc
          )
        in
          if i == -1
          then nothing()
          else 
          let ft::TensorFormatItem = head(tm:lookup(nm, fmt))
          in
          if case getElem(ft.specifiers, i) of
             | nothing() -> false
             | just(i) -> i == storeDense
             end
          then let j::Integer =
                 positionOf(
                   \ x::Integer
                     y::Integer
                   -> x == y
                   ,
                   i,
                   ft.dimenOrder
                 )
               in just(pair(nm.name, j + 1))
               end
          else nothing()
          end
        end
    | add(l, r) -> 
        case findDenseDimensionExpr(l, var, fmt) of
        | nothing() -> findDenseDimensionExpr(r, var, fmt)
        | x -> x
        end
    | sub(l, r) ->
        case findDenseDimensionExpr(l, var, fmt) of
        | nothing() -> findDenseDimensionExpr(r, var, fmt)
        | x -> x
        end
    | mul(l, r) -> 
        case findDenseDimensionExpr(l, var, fmt) of
        | nothing() -> findDenseDimensionExpr(r, var, fmt)
        | x -> x
        end
    | div(l, r) ->
        case findDenseDimensionExpr(l, var, fmt) of
        | nothing() -> findDenseDimensionExpr(r, var, fmt)
        | x -> x
        end
    | _ -> nothing()
    end;
}

function findAccesses
[Pair<String Integer>] ::= expr::TensorAssignExpr var::String
{
  return
    findAccessesAssign(expr, var) 
    ++
    findAccessesExpr(expr.tensorValue, var);
}

function findAccessesAssign
[Pair<String Integer>] ::= expr::TensorAssignExpr var::String
{
  return
    case expr.tensorAssign of
    | access(nm, acc) ->
        let ind::Integer =
          positionOf(
            stringEq(_, _),
            var,
            acc
          )
        in
        if ind == -1
        then []
        else [pair(nm.name, ind)]
        end
    | _ -> []
    end;
}

function findAccessesExpr
[Pair<String Integer>] ::= expr::TensorExpr var::String
{
  return
    case expr of
    | nullTensorExpr() -> []
    | access(nm, acc) ->
        let ind::Integer =
          positionOf(
            stringEq(_, _),
            var,
            acc
          )
        in
        if ind == -1
        then []
        else [pair(nm.name, ind)]
        end
    | tExpr(_) -> []
    | add(l, r) -> 
        findAccessesExpr(l, var) ++
        findAccessesExpr(r, var)
    | sub(l, r) ->
        findAccessesExpr(l, var) ++
        findAccessesExpr(r, var)
    | mul(l, r) ->
        findAccessesExpr(l, var) ++
        findAccessesExpr(r, var)
    | div(l, r) ->
        findAccessesExpr(l, var) ++
        findAccessesExpr(r, var)
    | _ -> []
    end;
}

function getExprs
[Expr] ::= e::TensorExpr
{
  return
    case e of
    | nullTensorExpr() -> []
    | access(_, _) -> []
    | tExpr(ex) -> [ex]
    | add(l, r) -> getExprs(l) ++ getExprs(r)
    | sub(l, r) -> getExprs(l) ++ getExprs(r)
    | mul(l, r) -> getExprs(l) ++ getExprs(r)
    | div(l, r) -> getExprs(l) ++ getExprs(r)
    end;
}

function sparse_assign
[Pair<String Integer>] ::= expr::TensorAssignExpr var::String
{
  return
    case expr.tensorAssign of
    | access(nm, acc) -> 
        let idx::Integer = 
          positionOf(
            stringEq(_, _),
            var,
            acc
          )
        in
        if idx == -1
        then []
        else 
        let ft::TensorFormatItem = head(tm:lookup(nm, expr.tensorFormat))
        in
        case getElem(ft.specifiers, idx) of
        | nothing() -> []
        | just(x) -> 
            if x == storeSparse
            then let j::Integer =
                   positionOf(
                     \ x::Integer
                       y::Integer
                     -> x == y
                     ,
                     idx,
                     ft.dimenOrder
                   )
              in [pair(nm.name, j + 1)]
              end
            else []
        end
        end
        end
    | _ -> []
    end;
}
