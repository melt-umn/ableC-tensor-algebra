grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute conds :: [TensorCond];
synthesized attribute exprs :: [TensorExpr];
synthesized attribute ifCnd :: [[TensorCond]];
synthesized attribute frthr :: [[ComputeGraph]];
synthesized attribute compute :: String;
synthesized attribute var :: String;

nonterminal ComputeGraph with conds, exprs, ifCnd, frthr, compute, var;

abstract production nullGraph
top::ComputeGraph ::= 
{
  top.conds = [];
  top.exprs = [];
  top.ifCnd = [];
  top.frthr = [];
  top.compute = "";
  top.var = "";
}

abstract production computeGraph
top::ComputeGraph ::=
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  vars::[String] loc::Location env::Decorated Env 
{
  local isBelowOut::Boolean =
    !containsAny(
      stringEq,
      tail(vars),
      head(assign.accesses)
    );

  local lp::LatticePoint =
    lattice_points(assign, fmts, value, head(vars), loc, env, true);

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
      listSubs(_, head(vars), tail(vars)),
      top.exprs
    );

  top.frthr = 
    if listLength(vars) == 1
    then
      map(
        \ e::TensorExpr ->
          []
        ,
        top.exprs
      )
    else
      map(
        \ e::TensorExpr ->
          let lattice::LatticePoint =
            lattice_points(
              assign, fmts, 
              -:q-makeSubs(
                e, head(vars), 
                tail(vars), 
                isBelowOut
              ), head(vars), 
              loc, env, false
            )
          in
          let pnts::[LatticePoint] =
            extractPoints(lattice)
          in
          map(
            \ lp::LatticePoint ->
              computeGraph(
                assign, fmts,
                lp.value,
                tail(vars), loc, 
                env
              )
            ,
            pnts
          )
          end
          end
        ,
        top.exprs
      );

  top.ifCnd =
    if listLength(vars) == 1
    then 
      map(
        \ e::TensorExpr ->
          []
        ,
        top.exprs
      )
    else
      map(
        \ e::TensorExpr ->
          let lattice::LatticePoint =
            lattice_points(
              assign, fmts, 
              makeSubs(
                e, head(vars),
                tail(vars),
                isBelowOut
              ), head(vars),
              loc, env, false
            )
          in
          let pnts::[LatticePoint] =
            extractPoints(lattice)
          in
          map(
            \ lp::LatticePoint ->
              lp.cond
            ,
            pnts
          )
          end
          end
        ,
        top.exprs
      );

  top.var = head(vars);

  top.compute = 
    implode("\n",
      zip4(
        \ c::TensorCond e::TensorExpr cd::[TensorCond] gr::[ComputeGraph] ->
          generateCode(c, e, cd, gr, head(vars), fmts)
        ,
        top.conds,
        top.exprs,
        top.ifCnd,
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

  return
    if e.isAvail && !isExpr(e)
    then [pair(s"t${var}${if idx == 0 then "" else toString(idx)}", e)]
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
TensorExpr ::= e::TensorExpr var::String remain::[String] isBelowOut::Boolean
{
  return makeSubs_helper(e, var, remain, isBelowOut, 0).fst;
}

function makeSubs_helper
Pair<TensorExpr Integer> ::= e::TensorExpr var::String remain::[String] isBelowOut::Boolean idx::Integer
{
  e.remaining = remain;

  return
    if e.isAvail && !isExpr(e)
    then
      pair(
        tensorBaseExpr(
          declRefExpr(
            name(s"t${var}${if idx == 0 then "" else toString(idx)}", location=e.location),
            location=e.location
          ),
          e.envr,
          location=e.location
        ),
        idx+1
      )
    else
      case e of
      | tensorAdd(ex, l, r, en) ->
        if l.isAvail && !isExpr(l) && isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && !isExpr(r) && isBelowOut
        then pair(l, idx+1)
        else 
          let sL::Pair<TensorExpr Integer> =
            makeSubs_helper(l, var, remain, isBelowOut, idx)
          in let sR::Pair<TensorExpr Integer> =
            makeSubs_helper(r, var, remain, isBelowOut, sL.snd)
          in
          pair(tensorAdd(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorSub(ex, l, r, en) ->
        if l.isAvail && !isExpr(l) && isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && !isExpr(r) && isBelowOut
        then pair(l, idx+1)
        else
          let sL::Pair<TensorExpr Integer> =
            makeSubs_helper(l, var, remain, isBelowOut, idx)
          in let sR::Pair<TensorExpr Integer> =
            makeSubs_helper(r, var, remain, isBelowOut, sL.snd)
          in
          pair(tensorSub(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorMul(ex, l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx)
        in let sR::Pair<TensorExpr Integer> =
          makeSubs_helper(r, var, remain, false, sL.snd)
        in
        pair(tensorMul(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
        end
        end
      | tensorDiv(ex, l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx)
        in let sR::Pair<TensorExpr Integer> = 
          makeSubs_helper(r, var, remain, false, sL.snd)
        in
        pair(tensorDiv(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
        end
        end
      | _ -> pair(e, idx)
      end;
}

function isExpr
Boolean ::= e::TensorExpr
{
  return
    case e of
    | tensorBaseExpr(_, _) -> true
    | _ -> false
    end;
}

function exprToString
String ::= e::TensorExpr
{
  return
    case e of
    | tensorBaseExpr(e, _) -> show(100, e.pp)
    | tensorAccess(_, t, i, _) -> show(100, t.pp) ++ "[" ++ show(100, i.pp) ++ "]"
    | tensorAdd(_, l, r, _) -> "(" ++ exprToString(l) ++ "+" ++ exprToString(r) ++ ")"
    | tensorSub(_, l, r, _) -> "(" ++ exprToString(l) ++ "-" ++ exprToString(r) ++ ")"
    | tensorMul(_, l, r, _) -> "(" ++ exprToString(l) ++ "*" ++ exprToString(r) ++ ")"
    | tensorDiv(_, l, r, _) -> "(" ++ exprToString(l) ++ "/" ++ exprToString(r) ++ ")"
    end;
}

function generateFullCondition
String ::= c::TensorCond e::TensorExpr var::String fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;
  e.variable = var;

  return
    c.condition
    ++
    case c of
    | allCond(_) ->
      if null(e.sparse)
      then ""
      else
        "&&"
        ++
        "("
        ++
        implode(
          "&&",
          map(
            \ p::Pair<String Integer> ->
              s"(p${p.fst}${toString(p.snd+1)} < ${p.fst}${toString(p.snd+1)}_pos[${if p.snd == 0 then "1" else s"p${p.fst}${toString(p.snd)} + 1"}])"
            ,
            e.sparse
          )
        )
        ++
        ")"
    | denseAccess(_, _, _) ->
      if null(e.sparse)
      then ""
      else
        "&&"
        ++
        "("
        ++
        implode(
          "&&",
          map(
            \ p::Pair<String Integer> ->
              s"(p${p.fst}${toString(p.snd+1)} < ${p.fst}${toString(p.snd+1)}_pos[${if p.snd == 0 then "1" else s"p${p.fst}${toString(p.snd)} + 1"}])"
            ,
            e.sparse
          )
        )
        ++
        ")"
    | _ -> ""
    end;
}

function generateCode
String ::= 
  c::TensorCond e::TensorExpr ic::[TensorCond] 
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
{
  local forLoop::Boolean =
    case c of
    | allCond(_) -> true
    | denseAccess(_, _, _) -> true
    | sparseAccess(_, _, _) -> true
    | _ -> false
    end;

  local forVar::String =
    case c of
    | allCond(v) -> v
    | denseAccess(_, _, v) -> v
    | sparseAccess(n, d, _) -> s"p${n}${toString(d+1)}"
    | _ -> ""
    end;

  return
    (
    if forLoop
    then
      s"for(unsigned long ${forVar} = 0; ${generateFullCondition(c, e, v, fmts)}; ${forVar}++) {"
    else
      s"while(${generateFullCondition(c, e, v, fmts)}) {"
    )
    ++
    "\n  expr = "
    ++
    exprToString(e)
    ++
    "\n  tensor = "
    ++
    e.tensorName
    ++
    "\n  "
    ++
    implode(
      "\n  ",
      explode(
        "\n",
        implode("\n",
          zipWith(
            \ c::TensorCond g::ComputeGraph ->
              s"if(${c.ifCond}) {"
              ++
              "\n  "
              ++
              implode(
                "\n  ",
                explode(
                  "\n",
                  g.compute
                )
              )
              ++
              "\n"
              ++
              "}"
            ,
            ic,
            g
          )
        )
      )
    )
    ++
    "\n}";
}
