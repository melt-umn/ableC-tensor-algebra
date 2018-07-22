grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute conds :: [TensorCond];
synthesized attribute exprs :: [TensorExpr];
synthesized attribute frthr :: [ComputeGraph];
synthesized attribute compute :: String;
synthesized attribute var :: String;

nonterminal ComputeGraph with conds, exprs, frthr, compute, var;

abstract production nullGraph
top::ComputeGraph ::= 
{
  top.conds = [];
  top.exprs = [];
  top.frthr = [];
  top.compute = "";
  top.var = "";
}

abstract production computeGraph
top::ComputeGraph ::=
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  vars::[String] loc::Location env::Decorated Env tensorNames::[String]
{
  local isOut::Boolean =
    !containsAny(
      stringEq,
      tail(vars),
      head(assign.accesses)
    )
    &&
    containsBy(
      stringEq,
      head(vars),
      head(assign.accesses)
    );

  local lp::LatticePoint =
    lattice_points(assign, fmts, value, head(vars), loc, env, tensorNames);

  local pnts::[LatticePoint] =
    extractPoints(lp);

  local filtered::[LatticePoint] =
    filterHead(
      \ c::LatticePoint h::[LatticePoint] ->
        let cond::TensorCond =
          optimizeCond(c.cond)
        in
        let conds::[TensorCond] =
          map(
            \ n::LatticePoint ->
              optimizeCond(n.cond),
            h
          )
        in
        !containsWith(
          \ con::TensorCond ->
            condIsAbove(con, cond)
          ,
          conds
        )
        end
        end
      ,
      pnts 
    );

  top.conds = 
    map(
      \ p::LatticePoint ->
        optimizeCond(p.cond)
      ,
      filtered
    );

  top.exprs =
    map(
      \ p::LatticePoint ->
        p.value
      ,
      filtered
    );

  local sbs :: [[Pair<String TensorExpr>]] =
    map(
      listSubs(_, tail(vars)),
      top.exprs
    );

  top.frthr = 
    if listLength(vars) == 1
    then
      map(
        \ e::TensorExpr ->
          nullGraph()
        ,
        top.exprs
      )
    else
      map(
        \ e::TensorExpr ->
          computeGraph(assign, fmts, makeSubs(e, tail(vars)), isOut, tail(vars), loc, env, tensorNames)
        ,
        top.exprs
      );

  top.var = head(vars);

  top.compute = 
    implode("\n",
      zip3(
        \ c::TensorCond e::TensorExpr gr::ComputeGraph ->
          c.condition
          ++
          "\n"
          ++
          gr.compute
        ,
        top.conds,
        top.exprs,
        top.frthr
      )
    );
}

function extractPoints
[LatticePoint] ::= p::LatticePoint
{
  return
    p ::
    flatMap(
      \ pnt::LatticePoint
      -> extractPoints(pnt)
      ,
      p.pnts
    );
}

function listSubs
[Pair<String TensorExpr>] ::= e::TensorExpr var::String remain::[String]
{
  return listSubs_helper(e, var, remain, 0);
}

function listSubs_helper
[Pair<String TensorExpr>] ::= e::TensorExpr var::String remain::[String] idx::Integer
{
  e.remaining = remain;

  local isExpr :: Boolean =
    case e of
    | tensorBaseExpr(_, _) -> true
    | _ -> false
    end;

  return
    if e.isAvail && !isExpr
    then pair(s"t${var}${if idx == 0 then "" else toString(idx)}", e)
    else
      case e of
      | tensorAdd(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] = 
          listSubs_helper(l, var, remain, idx)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL))
        in
        sbsL ++ sbsR
        end
        end
      | tensorSub(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL))
        in
        sbsL ++ sbsR
        end
        end
      | tensorMul(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL))
        in
        sbsL ++ sbsR
        end
        end
      | tensorDiv(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL))
        in
        sbsL ++ sbsR
        end
        end
      | _ -> []
      end;
}

function makeSubs
TensorExpr ::= e::TensorExpr var::String remain::[String] isOut::Boolean
{
  return makeSubs_helper(e, var, remain, isOut, 0).fst;
}

function makeSubs_helper
Pair<TensorExpr Integer> ::= e::TensorExpr var::String remain::[String] idx::Integer
{
  e.remaining = remain;
}
