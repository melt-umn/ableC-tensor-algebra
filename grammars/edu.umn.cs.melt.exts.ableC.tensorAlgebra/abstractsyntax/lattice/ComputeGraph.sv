grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute conds :: [TensorCond];
synthesized attribute exprs :: [[TensorExpr]];
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
        foldl(
          \ b::Boolean lat::LatticePoint ->
            b || 
            let e::Decorated TensorExpr =
              decorate c.value with {variable=head(vars); fmts=fmts;}
            in
            let ex::Decorated TensorExpr =
              decorate lat.value with {variable=head(vars); fmts=fmts;}
            in
            listLength(e.sparse) < listLength(ex.sparse)
            end
            end
          ,
          false,
          h
        )
        --||
        --!null(decorate c.value with {variable=head(vars); fmts=fmts;}.sparse)
        ||
        !containsWith(
          \ lat::LatticePoint ->
            condIsAbove(optimizeCond(lat.cond), cond)
          ,
          h
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

  local sbLts :: [[LatticePoint]] =
    map(
      \ e::LatticePoint ->
         let lattice::LatticePoint =
           lattice_points(
             assign, fmts,
             e.value, head(vars),
             loc, env, false
           )
          in 
          let pnts::[LatticePoint] =
            extractPoints(lattice)
          in
          filterHead(
            \ c::LatticePoint h::[LatticePoint] ->
              !containsWith(
                \ lat::LatticePoint ->
                  condIsAbove(lat.cond, c.cond)
                ,
                h
              )
            ,
            pnts
          )
          end
          end
      ,
      filtered
    );

  top.exprs =
    map(
      \ lst::[LatticePoint] ->
        map(
          \ l::LatticePoint ->
            l.value
          ,
          lst
        )
      ,
      sbLts
    );

  top.frthr = 
    if listLength(vars) == 1
    then
      map(
        \ lst::[LatticePoint] ->
          map(
            \ l::LatticePoint ->
              nullGraph()
            ,
            lst
          )
        ,
        sbLts
      )
    else
      map(
        \ lst::[LatticePoint] ->
          map(
            \ lp::LatticePoint ->
              computeGraph(
                assign, fmts, 
                makeSubs(
                  lp.value, head(vars),
                  tail(vars), isBelowOut
                ),
                tail(vars), loc,
                env
              )
            ,
            lst
          )
        ,
        sbLts
      );

  top.ifCnd =
    map(
      \ lst::[LatticePoint] ->
        map(
          \ lp::LatticePoint ->
            lp.cond
          ,
          lst
        )
      ,
      sbLts
    );

  top.var = head(vars);

  local exs::[TensorExpr] =
    map(
      \ p::LatticePoint ->
        p.value
      ,
      filtered
    );

  top.compute =
    if null(top.conds)
    then ""
    else
      generateCode(head(top.conds), head(exs), head(top.exprs), head(top.ifCnd), head(top.frthr), head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), true)
      ++
      "\n"
      ++
      implode("\n",
        zip5(
          \ c::TensorCond ex::TensorExpr e::[TensorExpr] cd::[TensorCond] gr::[ComputeGraph] ->
            generateCode(c, ex, e, cd, gr, head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), false)
          ,
          tail(top.conds),
          tail(exs),
          tail(top.exprs),
          tail(top.ifCnd),
          tail(top.frthr)
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
        if l.isAvail && {-!isExpr(l) &&-} isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && {-!isExpr(r) &&-} isBelowOut
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
        if l.isAvail && {-!isExpr(l) &&-} isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && {-!isExpr(r) &&-} isBelowOut
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

function reduceExpr
TensorExpr ::= e::TensorExpr var::String remain::[String] isBelowOut::Boolean env::Decorated Env
{
  return
    let res::TensorExpr =
      reduceExpr_helper(e, var, remain, isBelowOut)
    in
    collapseBaseExpr(res, last(remain), false, env).fst
    end;
}

function collapseBaseExpr
Pair<TensorExpr Boolean> ::= e::TensorExpr last::String found::Boolean env::Decorated Env
{
  return
    case e of
    | tensorBaseExpr(en, _) ->
      case decorate en with {env=env; returnType=nothing();} of
      | declRefExpr(n) ->
        let str :: String =
          n.name
        in
        if str == s"t${last}"
        then 
          if found
          then 
            pair(
              tensorBaseExpr(
                mkIntConst(0, e.location),
                e.envr, 
                location=e.location
              ), 
              true
            )
          else pair(e, true)
        else
         pair(e, found)
        end
      | _ -> pair(e, found)
      end
    | tensorAdd(ex, l, r, en) ->
      let lE::Pair<TensorExpr Boolean> =
        collapseBaseExpr(l, last, found, env)
      in
      let rE::Pair<TensorExpr Boolean> =
        collapseBaseExpr(r, last, lE.snd, env)
      in
      case lE.fst, rE.fst of
      | tensorBaseExpr(e1, _), tensorBaseExpr(e2, _) ->
        case decorate e1 with {env=env; returnType=nothing();},
             decorate e2 with {env=env; returnType=nothing();}
        of
        | mkIntConst(0, _), mkIntConst(0, _) ->
          pair(
            tensorBaseExpr(
              mkIntConst(0, e.location),
              e.envr,
              location=e.location
            ),
            rE.snd
          )
        | mkIntConst(0, _), _ ->
          rE
        | _, mkIntConst(0, _) ->
          lE
        | _, _ ->
          pair(
            tensorSub(ex, lE.fst, rE.fst, en, location=e.location),
            rE.snd
          )
        end
      | _, _ ->      
        pair(
          tensorAdd(ex, lE.fst, rE.fst, en, location=e.location),
          rE.snd
        )
      end
      end
      end
    | tensorSub(ex, l, r, en) ->
      let lE::Pair<TensorExpr Boolean> =
        collapseBaseExpr(l, last, found, env)
      in
      let rE::Pair<TensorExpr Boolean> =
        collapseBaseExpr(r, last, lE.snd, env)
      in
      case lE.fst, rE.fst of
      | tensorBaseExpr(e1, _), tensorBaseExpr(e2, _) ->
        case decorate e1 with {env=env; returnType=nothing();}, 
             decorate e2 with {env=env; returnType=nothing();}
        of
        | mkIntConst(0, _), mkIntConst(0, _) ->          
          pair(
            tensorBaseExpr(
              mkIntConst(0, e.location),
              e.envr,
              location=e.location
            ),
            rE.snd
          )
        | mkIntConst(0, _), _ ->
          rE
        | _, mkIntConst(0, _) ->
          lE
        | _, _ ->
          pair(
            tensorSub(ex, lE.fst, rE.fst, en, location=e.location),
            rE.snd
          )
        end
      | _, _ ->
        pair(
          tensorSub(ex, lE.fst, rE.fst, en, location=e.location), 
          rE.snd
        )
      end
      end
      end
    | _ -> pair(e, found)
    end;
}

function reduceExpr_helper
TensorExpr ::= e::TensorExpr var::String remain::[String] isBelowOut::Boolean
{
  e.remaining = remain;

  local base :: TensorExpr =
    tensorBaseExpr(
      declRefExpr(
        name(s"t${last(remain)}", location=e.location),
        location=e.location
      ),
      e.envr,
      location=e.location
    );

  return
    if null(remain) || isExpr(e)
    then e
    else
      if e.isAvail 
      then
        e
      else
        case e of
        | tensorAdd(ex, l, r, en) ->
          if (decorate l with {remaining=remain;}).isAvail
          then tensorAdd(ex, l, base, en, location=e.location)
          else if (decorate r with {remaining=remain;}).isAvail
          then tensorAdd(ex, base, r, en, location=e.location)
          else base
        | tensorSub(ex, l, r, en) ->
          if (decorate l with {remaining=remain;}).isAvail
          then tensorAdd(ex, l, base, en, location=e.location)
          else if (decorate r with {remaining=remain;}).isAvail
          then tensorAdd(ex, base, r, en, location=e.location)
          else base
        | _ -> base
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

function evalExpr
String ::= e::TensorExpr
{
  return
    case e of
    | tensorBaseExpr(_, _) -> e.exprName
    | tensorAccess(_, _, _, _) -> 
      e.tensorName ++ "_data[p" ++ e.tensorName ++ toString(listLength(head(e.accesses))) ++ "]"
    | tensorAdd(_, l, r, _) -> "(" ++ evalExpr(l) ++ "+" ++ evalExpr(r) ++ ")"
    | tensorSub(_, l, r, _) -> "(" ++ evalExpr(l) ++ "-" ++ evalExpr(r) ++ ")"
    | tensorMul(_, l, r, _) -> "(" ++ evalExpr(l) ++ "*" ++ evalExpr(r) ++ ")"
    | tensorDiv(_, l, r, _) -> "(" ++ evalExpr(l) ++ "/" ++ evalExpr(r) ++ ")"
    end;
}

function evalOut
String ::= e::TensorExpr
{
  return
    case e of
    | tensorBaseExpr(ex, _) ->
      case decorate ex with {env=e.envr; returnType=nothing();} of
      | declRefExpr(nm) -> nm.name
      | _ -> "__error"
      end
    | tensorAccess(_, _, _, _) ->
      e.tensorName ++ "_data[p" ++ e.tensorName ++ toString(listLength(head(e.accesses))) ++ "]"
    | _ -> "__error"
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
  c::TensorCond ex::TensorExpr e::[TensorExpr] ic::[TensorCond] 
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
  canEmitFor::Boolean assign::TensorExpr remain::[String]
  top::Boolean
{
  local subs::[Pair<String TensorExpr>] =
    listSubs(ex, v, remain);

  local forLoop::Boolean =
    canEmitFor 
    &&
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

  local emitElse::Boolean =
    case c of
    | sparseAccess(_, _, _) -> listLength(ic) == 1
    | _ -> false
    end;

  ex.variable = v;
  ex.fmts = fmts;

  local below::Boolean =
    case assign of
    | tensorAccess(_, _, _, _) ->
      let acc::[String] =
        head(assign.accesses)
      in
      !containsAny(
        stringEq, 
        v :: remain,
        acc
      )
      end
    | _ -> true
    end;

  local output::Boolean =
    case assign of
    | tensorAccess(_, _, _, _) ->
      let acc::[String] =
        head(assign.accesses)
      in
      !containsAny(
        stringEq, 
        remain,
        acc
      )
      &&
      containsBy(
        stringEq,
        v,
        acc
      )
      end
    | _ -> false
    end;

  local above :: Boolean =
    !output && !below;

  assign.variable = v;
  assign.fmts = fmts;

  local outSparse::Maybe<Pair<String Integer>> =
    case assign of
    | tensorAccess(_, _, _, _) ->
      let lst::[Pair<String Integer>] =
        assign.sparse
      in
      if null(lst)
      then nothing()
      else just(head(lst))
      end
    | _ -> nothing()
    end;

  local outDense::Maybe<Pair<String Integer>> =
    case assign of
    | tensorAccess(_, _, _, _) ->
      let lst::[Pair<String Integer>] =
        assign.dense
      in
      if null(lst)
      then nothing()
      else just(head(lst))
      end
    | _ -> nothing()
    end;

  local topAll::Boolean =
    case c of
    | allCond(_) -> true
    | denseAccess(_, _, _) -> true
    | _ -> false
    end;


  return
    (
    if top
    then
      implode("\n",
        map(
          \ p::Pair<String Integer> ->
            if forLoop && p.fst == forVar
            then ""
            else
              s"unsigned long p${p.fst}${toString(p.snd+1)} = ${p.fst}${toString(p.snd+1)}_pos[${if p.snd == 0 then "0" else s"p${p.fst}${toString(p.snd)}"}];"
          ,
          ex.sparse
        )
      )
    else ""
    )
    ++
    "\n"
    ++
    (
    if top
    then
      case outSparse of
      | just(pair(s, d)) ->
        s"unsigned long p${s}${toString(d+1)} = ${s}${toString(d+1)}_pos[${if d == 0 then "0" else s"p${s}${toString(d)}"}];"
      | nothing() -> ""
      end
    else ""
    )
    ++
    "\n"
    ++
    (
    if top && !forLoop && topAll
    then
      s"unsigned long ${v} = 0;"
      ++
      "\n"
    else ""
    )
    ++
    (
    if forLoop
    then
      s"for(unsigned long ${forVar} = 0; ${generateFullCondition(c, ex, v, fmts)}; ${forVar}++) {"
    else
      s"while(${generateFullCondition(c, ex, v, fmts)}) {"
    )
    ++
    "\n  "
    ++
    (
    if listLength(ex.sparse) == 1 && (!forLoop || forVar != v) && !topAll
    then 
      let p::Pair<String Integer> =
        head(ex.sparse)
      in
      s"unsigned long ${v} = ${p.fst}${toString(p.snd+1)}_idx[p${p.fst}${toString(p.snd+1)}];"
      end
    else
      implode("\n  ",
        map(
          \ p::Pair<String Integer> ->
            s"unsigned long ${v}${p.fst} = ${p.fst}${toString(p.snd+1)}_idx[p${p.fst}${toString(p.snd+1)}];"
          ,
          ex.sparse
        )
      )
      ++
      "\n  "
      ++
      (
      if null(ex.sparse) || (forLoop && forVar == v) || topAll
      then ""
      else
        s"unsigned long ${v} = ${generateMin(ex.sparse, v)};"
      )
    )
    ++
    "\n  "
    ++
    implode("\n  ",
      map(
        \ p::Pair<String Integer> ->
          s"unsigned long p${p.fst}${toString(p.snd+1)} = ${if p.snd == 0 then "0" else s"(p${p.fst}${toString(p.snd)} * ${p.fst}${toString(p.snd+1)}_size)"} + ${v};"
        ,
        ex.dense
      )
    )
    ++
    "\n  "
    ++
    case outDense of
    | just(pair(s, d)) ->
      s"unsigned long p${s}${toString(d+1)} = ${if d == 0 then "0" else s"(p${s}${toString(d)} * ${s}${toString(d+1)}_size)"} + ${v};"
    | nothing() -> ""
    end
    ++
    (
    if above
    then
      "\n  "
      ++
      implode("\n  ",
        map(
          \ p::Pair<String TensorExpr> ->
            s"double ${p.fst} = ${evalExpr(p.snd)};"
          ,
          subs
        )
      )
    else ""
    )
    ++
    (
    if output && !null(remain)
    then 
      "\n  "
      ++
      s"double t${last(remain)} = 0.0;"
    else ""
    )
    ++
    "\n  if(0) {}"
    ++
    "\n  "
    ++
    implode(
      "\n  ",
      explode(
        "\n",
        implode("\n",
          zip3(
            \ c::TensorCond e::TensorExpr g::ComputeGraph ->
              (
              if c.ifCond == "1" || emitElse
              then
                "else {"
              else
                s"else if(${c.ifCond}) {"
              )
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
              (
              if below
              then
                s"\n  t${if null(remain) then v else last(remain)} = ${evalExpr(reduceExpr(e, v, remain, true, e.envr))}"
              else ""
              )
              ++
              (
              if output
              then
                s"\n  ${evalOut(assign)} += ${evalExpr(reduceExpr(e, v, remain, true, e.envr))}"
              else ""
              )
              ++
              "\n  "
              ++
              case outSparse of
              | just(pair(s, d)) ->
                s"p${s}${toString(d+1)}++;"
              | nothing() -> ""
              end
              ++
              "\n}"
            ,
            ic,
            e,
            g
          )
        )
      )
    )
    ++
    "\n  "
    ++
    (
    if listLength(ex.sparse) == 1 && (!forLoop || forVar != v) && !topAll
    then
      let p::Pair<String Integer> =
        head(ex.sparse)
      in
      s"p${p.fst}${toString(p.snd+1)}++;"
      end
    else
      implode(
        "\n  ",
        map(
          \ p::Pair<String Integer> ->
            s"if(${v}${p.fst} == ${v}) p${p.fst}${toString(p.snd+1)}++;"
          ,
          ex.sparse
        )
      )
    )
    ++
    (
    if !forLoop && topAll
    then
      "\n  "
      ++
      s"${v}++;"
    else
      ""
    )
    ++
    "\n}";
}

function generateMin
String ::= prs::[Pair<String Integer>] var::String
{
  return 
    case prs of
    | [] -> ""
    | p::[] -> s"${var}${p.fst}"
    | p::tl -> s"({unsigned min = ${generateMin(tl, var)}; ${var}${p.fst} < min ? ${var}${p.fst} : min;})"
    end;
}
