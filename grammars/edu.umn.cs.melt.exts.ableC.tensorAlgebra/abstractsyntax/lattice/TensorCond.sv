grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute condition :: String;

nonterminal TensorCond with condition;

abstract production allCond
top::TensorCond ::= v::String
{
  top.condition = s"(${v} < ${v}_dimensions)";
}

abstract production nullCond
top::TensorCond ::=
{
  top.condition = "0";
}

abstract production sparseAccess
top::TensorCond ::= tNm::String dim::Integer
{
  top.condition = s"(p${tNm}${toString(dim+1)} < ${tNm}${toString(dim+1)}_pos[${if dim == 0 then "1" else s"p${tNm}${toString(dim)} + 1"}])";
}

abstract production denseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
  top.condition = s"(${var} < ${tNm}${toString(dim+1)}_size)";
}

function accessCond
TensorCond ::= tNm::String dim::Integer var::String fmt::TensorFormat
{
  local trip::Maybe<Pair<Integer Pair<Integer Integer>>> =
    getElem(fmt.storage, dim);
  
  local type::Integer = 
    trip.fromJust.snd.snd;

  return
    if type == storeSparse
    then sparseAccess(tNm, dim)
    else denseAccess(tNm, dim, var);
}

abstract production andCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.condition = s"(${l.condition} && ${r.condition})";
}

function condAnd
TensorCond ::= l::TensorCond r::TensorCond
{
  return
    case l, r of
    | nullCond(), nullCond() -> nullCond()
    | nullCond(), _ -> r
    | _, nullCond() -> l
    | allCond(v), allCond(_) -> allCond(v)
    | allCond(_), _ -> r
    | _, allCond(_) -> l
    | _, _ -> andCond(l, r)
    end;
}

function condOr
TensorCond ::= l::TensorCond r::TensorCond
{
  return
    case l, r of
    | nullCond(), nullCond() -> nullCond()
    | nullCond(), _ -> r
    | _, nullCond() -> l
    | allCond(v), allCond(_) -> allCond(v)
    | allCond(_), _ -> l
    | _, allCond(_) -> r
    | _, _ -> andCond(l, r)
    end;
}
