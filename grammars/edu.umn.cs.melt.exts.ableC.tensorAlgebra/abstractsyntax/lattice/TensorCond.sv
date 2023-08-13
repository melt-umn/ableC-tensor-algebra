grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- The conditions used by Merge Lattices to represent how
   tensors are merged. It also generates the Expr and String
   used in emitting (we use condition :: String, when generating
   OpenMP code because their system is very picky about how the
   code looks). ifCond :: String is used in a few places to 
   check whether the condition is "1" -}

synthesized attribute cnd :: Expr; -- The condition used in the loop
synthesized attribute ifCondition :: Expr; -- The condition used in the if
synthesized attribute condition :: String;
synthesized attribute ifCond :: String;

tracked nonterminal TensorCond with condition, ifCond, cnd, ifCondition;

-- All for this variable (Expr in the expression)
abstract production allCond
top::TensorCond ::= v::String
{
  top.condition = s"${v} < ${v}_dimensions";
  top.cnd = ableC_Expr{ $name{v} < $name{s"${v}_dimensions"} };
  top.ifCond = "1";
  top.ifCondition = ableC_Expr { 1 };
}

-- Not accessed by this variable
abstract production nullCond
top::TensorCond ::=
{
  top.condition = "0";
  top.cnd = ableC_Expr { 0 };
  top.ifCond = "0";
  top.ifCondition = ableC_Expr { 0 };
}

-- A sparse access of the tensor with name tNm, at dimension dim, for variable v
abstract production sparseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
  top.condition = s"p${tNm}${toString(dim+1)} < ${tNm}${toString(dim+1)}_pos[${if dim == 0 then "1" else s"p${tNm}${toString(dim)} + 1"}]";
  top.cnd = ableC_Expr { $name{s"p${tNm}${toString(dim+1)}"} < $name{s"${tNm}${toString(dim+1)}_pos"}[$Expr{if dim == 0 then ableC_Expr{1} else ableC_Expr{$name{s"p${tNm}${toString(dim)}"} + 1}}] };
  top.ifCond = s"${var}${tNm} == ${var}";
  top.ifCondition = ableC_Expr { $name{s"${var}${tNm}"} == $name{var} };
}

-- A dense access
abstract production denseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
  top.condition = s"${var} < ${tNm}${toString(dim+1)}_size";
  top.cnd = ableC_Expr { $name{var} < $name{s"${tNm}${toString(dim+1)}_size"} };
  top.ifCond = "1";
  top.ifCondition = ableC_Expr { 1 };
}

-- Taking a tensor name, dimension, variable name, and format, automatically
-- determines the proper condition
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

-- Merge condition.
abstract production andCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.condition = s"(${l.condition}) && (${r.condition})";
  top.cnd = ableC_Expr { ( $Expr{l.cnd} && $Expr{r.cnd} ) };
  top.ifCond = s"(${l.ifCond}) && (${r.ifCond})";
  top.ifCondition = ableC_Expr { ( $Expr{l.ifCondition} && $Expr{r.ifCondition} ) };
}

{- We don't provide an orCond production, because we remove or 
   conditions and replace them with and's, and loops to finish
   off extra values, like described in Kjolstad. -}
function condAnd
TensorCond ::= l::TensorCond r::TensorCond loop::Boolean
{
  return
    if isNullCond(l) && isNullCond(r) -- null && null
    then nullCond()
    else if isNullCond(l) && isAllCond(r) -- null && all
    then nullCond()
    else if isAllCond(l) && isNullCond(r) -- null && all
    then nullCond()
    else if isNullCond(l) -- null && r
    then r
    else if isNullCond(r) -- l && null
    then l
    else if isAllCond(l) && isAllCond(r) -- all && all
    then l
    else if isAllCond(l) -- all && r
    then r
    else if isAllCond(r) -- l && all
    then l
    else if isDenseCond(l) && isDenseCond(r) -- dense && dense
    then l
    else if isDenseCond(l) -- dense && r
    then r
    else if isDenseCond(r) -- l && dense
    then l
    else andCond(l, r); -- l && r
}

function condOr
TensorCond ::= l::TensorCond r::TensorCond loop::Boolean
{ -- How we merge with or is different if we're looking at a loop or an if
  return
    if loop
    then
      if isNullCond(l) && isNullCond(r) -- null || null
      then nullCond()
      else if isNullCond(l) -- null || r
      then r
      else if isNullCond(r) -- l || null
      then l
      else if isAllCond(l) && isAllCond(r) -- all || all
      then l
      else if isAllCond(l) -- all || r
      then l
      else if isAllCond(r) -- l || all
      then r
      else if isDenseCond(l) && isDenseCond(r) -- dense || dense
      then l
      else if isDenseCond(l) -- dense || r
      then l
      else if isDenseCond(r) -- l || dense
      then r
      else andCond(l, r) -- l || r
    else
      if isNullCond(l) && isNullCond(r) -- null || null
      then nullCond()
      else if isNullCond(l) -- null || r
      then r
      else if isNullCond(r) -- l || null
      then l
      else if isAllCond(l) && isAllCond(r) -- all || all
      then l
      else if isAllCond(l) -- all || r
      then r -- difference (can't drop r, must know for if)
      else if isAllCond(r) -- l || all
      then l -- difference (can't drop l, must know for if)
      else if isDenseCond(l) && isDenseCond(r) -- dense || dense
      then l
      else if isDenseCond(l) -- dense || r
      then r -- difference (can't drop r, must known for if)
      else if isDenseCond(r) -- l || dense
      then l -- difference (can't drop l, must known for if)
      else andCond(l, r); -- l || r
}

{- Optimize / cleanup a condition -}
function optimizeCond
TensorCond ::= c::TensorCond
{
  local dense::[TensorCond] =
    findDenseAccess(c); -- find the dense accesses

  -- remove uneeded dense conditions (all dense conditions are the same)
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

  {- If there's a dense access in the condition, we only need one 
     dense. The condition generators prevent a situation where we
     have (and need) sparse && dense -}
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
        andCond(oL, oR)
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

function isNullCond
Boolean ::= c::TensorCond
{
  return
    case c of
    | nullCond() -> true
    | _ -> false
    end;
}

function isDenseCond
Boolean ::= c::TensorCond
{
  return
    case c of
    | denseAccess(_, _, _) -> true
    | _ -> false
    end;
}

{- Used to determine if one condition totally
   covers another. This tests for:
     if(c) then ... else if (b) then ...never occurs...
   This allows for nicer code emission.
-}
function condIsAbove
Boolean ::= c::TensorCond b::TensorCond
{
  return
    case c, b of
    | allCond(_), allCond(_) -> false
    | allCond(_), _ -> true
    -- nullCond should never actually occur in the codegen
    | nullCond(), nullCond() -> true
    | nullCond(), _ -> true
    | _, nullCond() -> true
    | denseAccess(_, _, _), allCond(_) -> false
    | denseAccess(_, _, _), denseAccess(_, _, _) -> true
    | denseAccess(_, _, _), _ -> true
    | sparseAccess(n1, d1, v1), sparseAccess(n2, d2, v2) ->
      n1 == n2 && d1 == d2 && v1 == v2
    | _, _ -> false
    end;
}
