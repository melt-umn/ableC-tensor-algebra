grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute condition :: String;
synthesized attribute ifCond :: String;

nonterminal TensorCond with condition, ifCond;

abstract production allCond
top::TensorCond ::= v::String
{
  top.condition = s"(${v} < ${v}_dimensions)";
  top.ifCond = "1";
}

abstract production nullCond
top::TensorCond ::=
{
  top.condition = "0";
  top.ifCond = "0";
}

abstract production sparseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
  top.condition = s"(p${tNm}${toString(dim+1)} < ${tNm}${toString(dim+1)}_pos[${if dim == 0 then "1" else s"p${tNm}${toString(dim)} + 1"}])";
  top.ifCond = s"(${var}${tNm} == ${var})";
}

abstract production denseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
  top.condition = s"(${var} < ${tNm}${toString(dim+1)}_size)";
  top.ifCond = "1";
}

function accessCond
TensorCond ::= tNm::String dim::Integer var::String fmt::TensorFormat
{
  local trip::Maybe<Pair<Integer Pair<Integer Integer>>> =
    getElem(fmt.storage, dim);
  
  local type::Integer = 
    trip.fromJust.snd.snd;

  return
    if !trip.isJust
    then nullCond()
    else
      if type == storeSparse
      then sparseAccess(tNm, dim, var)
      else denseAccess(tNm, dim, var);
}

abstract production andCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.condition = s"(${l.condition} && ${r.condition})";
  top.ifCond = s"(${l.ifCond} && ${r.ifCond})";
}

function condAnd
TensorCond ::= l::TensorCond r::TensorCond loop::Boolean
{
  return
    case l, r of
    | nullCond(), nullCond() -> nullCond()
    | nullCond(), allCond(_) -> nullCond()
    | allCond(_), nullCond() -> nullCond()
    | nullCond(), _ -> r
    | _, nullCond() -> l
    | allCond(v), allCond(_) -> allCond(v)
    | allCond(_), _ -> r
    | _, allCond(_) -> l
    | denseAccess(_, _, _), denseAccess(_, _, _) -> l
    | denseAccess(_, _, _), _ -> r
    | _, denseAccess(_, _, _) -> l
    | _, _ -> andCond(l, r)
    end;
}

function condOr
TensorCond ::= l::TensorCond r::TensorCond loop::Boolean
{
  return
    if loop
    then
      case l, r of
      | nullCond(), nullCond() -> nullCond()
      | nullCond(), _ -> r
      | _, nullCond() -> l
      | allCond(v), allCond(_) -> allCond(v)
      | allCond(_), _ -> l
      | _, allCond(_) -> r
      | denseAccess(_, _, _), denseAccess(_, _, _) -> l
      | denseAccess(_, _, _), _ -> l
      | _, denseAccess(_, _, _) -> r
      | _, _ -> andCond(l, r)
      end
    else
      case l, r of
      | nullCond(), nullCond() -> nullCond()
      | nullCond(), _ -> r
      | _, nullCond() -> l
      | allCond(v), allCond(_) -> allCond(v)
      | allCond(_), _ -> r
      | _, allCond(_) -> l
      | denseAccess(_, _, v), denseAccess(_, _, _) -> allCond(v)
      | denseAccess(_, _, _), _ -> r
      | _, denseAccess(_, _, _) -> l
      | _, _ -> andCond(l, r)
      end;
}

function optimizeCond
TensorCond ::= c::TensorCond
{
  local dense::[TensorCond] =
    findDenseAccess(c);

  local denseAcc::[TensorCond] =
    if listLength(dense) > 1
    then
      let t::[TensorCond] =
        nubBy(
          condEqual, 
          dense
        )
      in
      if listLength(t) > 1
      then
        filter(
          \ c::TensorCond ->
            !isAllCond(c)
          ,
          t
        )
      else t
      end
    else dense;

  return
    if !null(dense)
    then head(denseAcc)
    else
      case c of
      | andCond(l, r) ->
        let oL::TensorCond =
          optimizeCond(l)
        in let oR::TensorCond =
          optimizeCond(r)
        in
        andCond(l, r)
        end
        end
      | _ -> c
      end;
}

function findDenseAccess
[TensorCond] ::= c::TensorCond
{
  return
    case c of
    | allCond(_) -> [c]
    | nullCond() -> []
    | sparseAccess(_, _, _) -> []
    | denseAccess(_, _, _) -> [c]
    | andCond(l, r) -> 
      findDenseAccess(l)
      ++
      findDenseAccess(r)
    end;
}

function condEqual
Boolean ::= a::TensorCond b::TensorCond
{
  return
    case a, b of
    | allCond(va), allCond(vb) -> va == vb
    | nullCond(), nullCond() -> true
    | sparseAccess(na, da, _), sparseAccess(nb, db, _) ->
      na == nb && da == db
    | denseAccess(na, da, va), denseAccess(nb, db, vb) ->
      na == nb && da == db && va == vb
    | andCond(la, ra), andCond(lb, rb) ->
      condEqual(la, lb) && condEqual(ra, rb)
    | _, _ -> false
    end;
}

function isAllCond
Boolean ::= c::TensorCond
{
  return
    case c of
    | allCond(_) -> true
    | _ -> false
    end;
}

function condIsAbove
Boolean ::= c::TensorCond b::TensorCond
{
  return
    case c, b of
    | allCond(_), allCond(_) -> false
    | allCond(_), _ -> true
    | nullCond(), nullCond() -> true
    | nullCond(), _ -> true
    | _, nullCond() -> true
    | denseAccess(_, _, _), allCond(_) -> false
    | denseAccess(_, _, _), denseAccess(_, _, _) -> false
    | denseAccess(_, _, _), _ -> true
    | sparseAccess(_, _, _), sparseAccess(_, _, _) -> false
    | _, _ -> false
    end;
}
