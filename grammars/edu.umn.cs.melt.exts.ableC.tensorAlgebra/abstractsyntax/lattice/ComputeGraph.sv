grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute conds :: [TensorCond];
synthesized attribute exprs :: [[TensorExpr]];
synthesized attribute ifCnd :: [[TensorCond]];
synthesized attribute frthr :: [[ComputeGraph]];
synthesized attribute asmbl :: String;
synthesized attribute compute :: String;
synthesized attribute var :: String;

nonterminal ComputeGraph with conds, exprs, ifCnd, frthr, asmbl, compute, var;

abstract production nullGraph
top::ComputeGraph ::= 
{
  top.conds = [];
  top.exprs = [];
  top.ifCnd = [];
  top.frthr = [];
  top.asmbl = "";
  top.compute = "";
  top.var = "";
}

abstract production computeGraph
top::ComputeGraph ::=
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  vars::[String] loc::Location env::Decorated Env
{
  assign.fmts = fmts;

  local outDense :: Boolean =
    allDense(getTensorFormat(assign, fmts));

  local isBelowOut::Boolean =
    !containsAny(
      stringEq,
      tail(vars),
      head(assign.accesses)
    );

  local above::Boolean =
    if null(assign.accesses)
    then false
    else
      containsBy(
        stringEq,
        last(head(assign.accesses)),
        tail(vars)
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
                (
                if above
                then
                  makeSubs(
                    lp.value, head(vars),
                    tail(vars), isBelowOut,
                    fmts
                  )
                else
                  lp.value
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

  top.asmbl =
    if outDense
    then ""
    else if null(top.conds)
    then ""
    else
      generateAssemble(head(top.conds), head(exs), head(top.exprs), head(top.ifCnd), head(top.frthr), head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), true)
      ++
      "\n"
      ++
      implode("\n",
        zip5(
          \ c::TensorCond ex::TensorExpr e::[TensorExpr] cd::[TensorCond] gr::[ComputeGraph] ->
            generateAssemble(c, ex, e, cd, gr, head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), false)
          ,
          tail(top.conds),
          tail(exs),
          tail(top.exprs),
          tail(top.ifCnd),
          tail(top.frthr)
        )
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
[Pair<String TensorExpr>] ::= 
  e::TensorExpr var::String remain::[String] fmts::tm:Map<String TensorFormat>
{
  return listSubs_helper(e, var, remain, 0, fmts);
}

function listSubs_helper
[Pair<String TensorExpr>] ::= 
  e::TensorExpr var::String remain::[String] idx::Integer
  fmts::tm:Map<String TensorFormat>
{
  e.remaining = remain;
  e.fmts = fmts;

  return
    if e.isAvail && !isExpr(e)
    then [pair(s"t${var}${if idx == 0 then "" else toString(idx)}", e)]
    else
      case e of
      | tensorAdd(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] = 
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorSub(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorMul(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorDiv(_, l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | _ -> []
      end;
}

function makeSubs
TensorExpr ::= 
  e::TensorExpr var::String remain::[String] isBelowOut::Boolean
  fmts::tm:Map<String TensorFormat>
{
  return makeSubs_helper(e, var, remain, isBelowOut, 0, fmts).fst;
}

function makeSubs_helper
Pair<TensorExpr Integer> ::= 
  e::TensorExpr var::String remain::[String] isBelowOut::Boolean 
  idx::Integer fmts::tm:Map<String TensorFormat>
{
  e.remaining = remain;
  e.fmts = fmts;

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
        if l.isAvail && isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && isBelowOut
        then pair(l, idx+1)
        else 
          let sL::Pair<TensorExpr Integer> =
            makeSubs_helper(l, var, remain, isBelowOut, idx, fmts)
          in let sR::Pair<TensorExpr Integer> =
            makeSubs_helper(r, var, remain, isBelowOut, sL.snd, fmts)
          in
          pair(tensorAdd(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorSub(ex, l, r, en) ->
        if l.isAvail && isBelowOut
        then pair(r, idx+1)
        else if r.isAvail && isBelowOut
        then pair(l, idx+1)
        else
          let sL::Pair<TensorExpr Integer> =
            makeSubs_helper(l, var, remain, isBelowOut, idx, fmts)
          in let sR::Pair<TensorExpr Integer> =
            makeSubs_helper(r, var, remain, isBelowOut, sL.snd, fmts)
          in
          pair(tensorSub(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorMul(ex, l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx, fmts)
        in let sR::Pair<TensorExpr Integer> =
          makeSubs_helper(r, var, remain, false, sL.snd, fmts)
        in
        pair(tensorMul(ex, sL.fst, sR.fst, en, location=e.location), sR.snd)
        end
        end
      | tensorDiv(ex, l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx, fmts)
        in let sR::Pair<TensorExpr Integer> = 
          makeSubs_helper(r, var, remain, false, sL.snd, fmts)
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

function evalExpr
String ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorBaseExpr(_, _) -> e.exprName
    | tensorAccess(_, _, _, _) -> 
      e.tensorName ++ "_data[p" ++ e.tensorName ++ toString(listLength(head(e.accesses))) ++ "]"
    | tensorAdd(_, l, r, _) -> 
      "(" ++ evalExpr(l, fmts) ++ "+" ++ evalExpr(r, fmts) ++ ")"
    | tensorSub(_, l, r, _) -> 
      "(" ++ evalExpr(l, fmts) ++ "-" ++ evalExpr(r, fmts) ++ ")"
    | tensorMul(_, l, r, _) -> 
      "(" ++ evalExpr(l, fmts) ++ "*" ++ evalExpr(r, fmts) ++ ")"
    | tensorDiv(_, l, r, _) -> 
      "(" ++ evalExpr(l, fmts) ++ "/" ++ evalExpr(r, fmts) ++ ")"
    end;
}

function evalOut
String ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

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
String ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorBaseExpr(e, _) -> show(100, e.pp)
    | tensorAccess(_, t, i, _) -> e.tensorName ++ "[" ++ implode(",", head(e.accesses)) ++ "]"
    | tensorAdd(_, l, r, _) -> 
      "(" ++ exprToString(l, fmts) ++ "+" ++ exprToString(r, fmts) ++ ")"
    | tensorSub(_, l, r, _) -> 
      "(" ++ exprToString(l, fmts) ++ "-" ++ exprToString(r, fmts) ++ ")"
    | tensorMul(_, l, r, _) -> 
      "(" ++ exprToString(l, fmts) ++ "*" ++ exprToString(r, fmts) ++ ")"
    | tensorDiv(_, l, r, _) -> 
      "(" ++ exprToString(l, fmts) ++ "/" ++ exprToString(r, fmts) ++ ")"
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

function generateForCondition
String ::= c::TensorCond e::TensorExpr var::String fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;
  e.variable = var;

  return
    c.condition;
}

function generateCode
String ::= 
  c::TensorCond ex::TensorExpr e::[TensorExpr] ic::[TensorCond] 
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
  canEmitFor::Boolean assign::TensorExpr remain::[String]
  top::Boolean 
{
  local subs::[Pair<String TensorExpr>] =
    listSubs(ex, v, remain, fmts);

  local forLoop::Boolean =
    canEmitFor 
    &&
    case c of
    | allCond(_) -> null(ex.sparse)
    | denseAccess(_, _, _) -> null(ex.sparse)
    | sparseAccess(_, _, _) -> true
    | _ -> false
    end;

  local canParallel :: Boolean =
    forLoop && !outSparse.isJust;

  local forVar::String =
    case c of
    | allCond(v) -> v
    | denseAccess(_, _, v) -> v
    | sparseAccess(n, d, _) -> s"p${n}${toString(d+1)}"
    | _ -> ""
    end;

  local forInit::String =
    case c of
    | allCond(v) -> s"unsigned long ${v} = 0"
    | denseAccess(_, _, v) -> s"unsigned long ${v} = 0"
    | sparseAccess(n, d, _) -> s"unsigned long p${n}${toString(d+1)} = ${n}${toString(d+1)}_pos[${if d == 0 then "0" else s"p${n}${toString(d)}"}]"
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

  local next_sparse :: Maybe<Pair<String Integer>> =
    assign.next_sparse;

  local topAll::Boolean =
    case c of
    | allCond(_) -> true
    | denseAccess(_, _, _) -> true
    | _ -> false
    end;


  local redSubs::[Pair<String TensorExpr>] =
    list_reduceDeeper(ex, v::remain, fmts);

  return
    (
    if top
    then
      implode("\n",
        map(
          \ p::Pair<String Integer> ->
            if forLoop && s"p${p.fst}${toString(p.snd+1)}" == forVar
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
      (
      if canParallel
      then "__parallel_emit;\n" 
      else ""
      ) ++
      s"for(${forInit}; ${generateForCondition(c, ex, v, fmts)}; ${forVar}++) {"
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
            s"double ${p.fst} = ${evalExpr(p.snd, fmts)};"
          ,
          subs
        )
      )
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
              let red::TensorExpr = 
                reduceDeeper(e, v, remain, fmts)
              in
              let sbs::[Pair<String TensorExpr>] =
                list_reduceDeeper(e, remain, fmts)
              in
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
              (
              if (output || below)
              && (decorate e with {remaining=remain; fmts=fmts;}).isAvail
              then ""
              else
                (
                if output || below
                then 
                  implode("\n  ",
                    map(
                      \ s::Pair<String TensorExpr> ->
                        s"double ${s.fst} = 0.0;"
                      ,
                      sbs
                    )
                  )
                else ""
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
              )
              ++
              (
              if below
              then
                implode("\n",
                  map(
                    \ p::Pair<String TensorExpr> ->
                      if exprContained(e, p.snd, fmts)
                      then
                        let sb::TensorExpr =
                          performSubs(p.snd, sbs, fmts)
                        in
                        s"${p.fst} += ${evalExpr(sb, fmts)};"
                        end
                      else
                        let possible::[TensorExpr] =
                          exprCanZero(p.snd)
                        in
                        let i::Integer =
                          positionBy(
                            \ ex::TensorExpr ->
                              exprContained(e, ex, fmts)
                            ,
                            possible
                          )
                        in
                        if i != -1
                        then
                          s"${p.fst} += ${evalExpr(getElem(possible, i).fromJust, fmts)};"
                        else ""
                        end
                        end
                    ,
                    redSubs
                  )
                )
              else ""
              )
              ++
              (
              if output
              then
              s"\n  ${evalOut(assign, fmts)} += ${evalExpr(red, fmts)};"
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
              end
              end
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
    if listLength(ex.sparse) == 1 && !topAll
    then
      let p::Pair<String Integer> =
        head(ex.sparse)
      in
      if forLoop && forVar == s"p${p.fst}${toString(p.snd+1)}"
      then ""
      else
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

function generateAssemble
String ::= 
  c::TensorCond ex::TensorExpr e::[TensorExpr] ic::[TensorCond]
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
  canEmitFor::Boolean assign::TensorExpr remain::[String]
  top::Boolean
{
  local fmtNm::String =
    getTensorFormat(assign, fmts).proceduralName;
  local oC::Integer =
    getTensorFormat(assign, fmts).dimensions
    -
    listLength(
      filter(
        \ s::String ->
          containsBy(
            stringEq, 
            s,
            remain
          )
        ,
        head(assign.accesses)
      )
    );

  local subs::[Pair<String TensorExpr>] =
    listSubs(ex, v, remain, fmts);

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

  local forInit::String =
    case c of
    | allCond(v) -> s"unsigned long ${v} = 0"
    | denseAccess(_, _, v) -> s"unsigned long ${v} = 0"
    | sparseAccess(n, d, _) -> s"unsigned long p${n}${toString(d+1)} = ${n}${toString(d+1)}_pos[${if d == 0 then "0" else s"p${n}${toString(d)}"}]"
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

  local doesOut::Boolean =
    case assign of
    | tensorAccess(_, _, _, _) ->
      !null(assign.dense ++ assign.sparse)
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
  if below
  then ""
  else
    (
    if top
    then
      implode("\n",
        map(
          \ p::Pair<String Integer> ->
            if forLoop && s"p${p.fst}${toString(p.snd+1)}" == forVar
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
      s"for(${forInit}; ${generateFullCondition(c, ex, v, fmts)}; ${forVar}++) {"
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
    (
    if doesOut 
    then
      s"\nidx[${toString(oC-1)}] = ${v};\n"
    else ""
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
              (
              if (output || below)
              && (decorate e with {remaining=remain; fmts=fmts;}).isAvail
              then ""
              else
                implode(
                  "\n  ",
                  explode(
                    "\n",
                    g.asmbl
                  )
                )
              )
              ++
              (
              if output
              then
                s"tensor_getPointer_${fmtNm}(&${assign.tensorName}, idx);"
              else ""
              )
              ++
              "\n  "
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
    if listLength(ex.sparse) == 1 && !topAll
    then
      let p::Pair<String Integer> =
        head(ex.sparse)
      in
      if forLoop && forVar == s"p${p.fst}${toString(p.snd+1)}"
      then ""
      else
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

function reduceDeeper
TensorExpr ::= 
  ex::TensorExpr var::String remain::[String] 
  fmts::tm:Map<String TensorFormat>
{
  return 
    if null(remain)
    then ex
    else 
      reduceDeeper_helper(ex, var, remain, 0, fmts).fst;
}

function list_reduceDeeper
[Pair<String TensorExpr>] ::= 
  ex::TensorExpr remain::[String]
  fmts::tm:Map<String TensorFormat>
{
  return
    if null(remain)
    then []
    else
      list_reduceDeeper_helper(ex, remain, 0, fmts).fst;
}

function list_reducedSubs
[Pair<String TensorExpr>] ::= 
  ex::TensorExpr remain::[String] fmts::tm:Map<String TensorFormat>
{
  return
    if null(remain)
    then []
    else
      list_reducedSubs_helper(ex, remain, 0, fmts).fst;
}

function reduceDeeper_helper
Pair<TensorExpr Integer> ::= 
  ex::TensorExpr var::String remain::[String] idx::Integer
  fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return
    if ex.isAvail 
    then pair(ex, idx)
    else
      case ex of
      | tensorBaseExpr(_, _) ->
        pair(ex, idx)
      | tensorAccess(_, _, _, en) ->
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(s"t${head(remain)}${toString(idx)}", location=ex.location),
              location=ex.location
            ),
            en,
            location=ex.location
          )
          ,
          idx+1
        )
      | tensorAdd(ex, l, r, en) ->
        reduceDeeper_function(
          tensorAdd(_, _, _, _, location=_), var, remain, 
          idx, ex, en, l, r, fmts, true
        )
      | tensorSub(ex, l, r, en) ->
        reduceDeeper_function(
          tensorSub(_, _, _, _, location=_), var, remain,
          idx, ex, en, l, r, fmts, true
        )
      | tensorMul(ex, l, r, en) ->
        reduceDeeper_function(
          tensorMul(_, _, _, _, location=_), var, remain, 
          idx, ex, en, l, r, fmts, false
        )
      | tensorDiv(ex, l, r, en) ->
        reduceDeeper_function(
          tensorDiv(_, _, _, _, location=_), var, remain,
          idx, ex, en, l, r, fmts, false
        )
      end;
}

function list_reduceDeeper_helper
Pair<[Pair<String TensorExpr>] Integer> ::= 
  ex::TensorExpr remain::[String] idx::Integer
  fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return
    if ex.isAvail
    then pair([], idx)
    else
      case ex of
      | tensorBaseExpr(_, _) ->
        pair([], idx)
      | tensorAccess(_, _, _, en) ->
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", ex)],
          idx+1
        )
      | tensorAdd(ep, l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, ep, en, l, r, ex, fmts, true
        )
      | tensorSub(ep, l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, ep, en, l, r, ex, fmts, true
        )
      | tensorMul(ep, l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, ep, en, l, r, ex, fmts, false
        )
      | tensorDiv(ep, l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, ep, en, l, r, ex, fmts, false
        )
      end;
}

function list_reducedSubs_helper
Pair<[Pair<String TensorExpr>] Integer> ::= 
  expr::TensorExpr remain::[String]
  idx::Integer fmts::tm:Map<String TensorFormat>
{
  expr.remaining = remain;
  expr.fmts = fmts;

  return
    if expr.isAvail
    then 
      pair([], idx)
    else
      case expr of
      | tensorBaseExpr(_, _) ->
        pair([], idx)
      | tensorAccess(_, _, _, _) ->
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", expr)],
          idx+1
        )
      | tensorAdd(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr, fmts
        )
      | tensorSub(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr, fmts
        )
      | tensorMul(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr, fmts
        )
      | tensorDiv(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr, fmts
        )
      end;
}

function reduceDeeper_function
Pair<TensorExpr Integer> ::= 
  prod::(TensorExpr ::= Expr TensorExpr TensorExpr Decorated Env Location)
  var::String remain::[String] idx::Integer ex::Expr en::Decorated Env
  l::TensorExpr r::TensorExpr fmts::tm:Map<String TensorFormat>
  addSub::Boolean
{
  l.remaining = remain;
  l.fmts = fmts;
  r.remaining = remain;
  r.fmts = fmts;
  
  return
    if addSub
    then -- add / sub
      if anyAvail(l, remain, fmts) && anyAvail(r, remain, fmts)
      then
        let lSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(l, var, remain, idx, fmts)
        in
        let rSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(r, var, remain, lSub.snd, fmts)
        in
        pair(
          prod(ex, lSub.fst, rSub.fst, en, ex.location),
          rSub.snd
        )
        end
        end
      else if anyAvail(l, remain, fmts)
      then
        let lSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(l, var, remain, idx, fmts)
        in
        let rSub::Pair<TensorExpr Integer> =
          pair(
            tensorBaseExpr(
              declRefExpr(
                name(
                  s"t${head(remain)}${toString(lSub.snd)}",
                  location=ex.location
                ),
                location=ex.location
              ),
              en,
              location=ex.location
            ),
            lSub.snd+1
          )
        in
        pair(
          prod(ex, lSub.fst, rSub.fst, en, ex.location),
          rSub.snd
        )
        end
        end
      else if anyAvail(r, remain, fmts)
      then 
        let lSub::Pair<TensorExpr Integer> =
          pair(
            tensorBaseExpr(
              declRefExpr(
                name(
                  s"t${head(remain)}${toString(idx)}",
                  location=ex.location
                ),
                location=ex.location
              ),
              en,
              location=ex.location
            ),
            idx+1
          )
        in
        let rSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(r, var, remain, lSub.snd, fmts)
        in
        pair(
          prod(ex, lSub.fst, rSub.fst, en, ex.location),
          rSub.snd
        )
        end
        end
      else
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(
                s"t${head(remain)}${toString(idx)}",
                location=ex.location
              ),
              location=ex.location
            ),
            en,
            location=ex.location
          )
          ,
          idx+1
      )
    else
      if l.isAvail && !availSimul(r, remain, fmts)
      then
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(
                s"t${head(remain)}${toString(idx)}",
                location=ex.location
              ),
              location=ex.location
            ),
            en,
            location=ex.location
          ),
          idx+1
        )
      else if l.isAvail
      then
        let rSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(r, var, remain, idx, fmts)
        in
        pair(
          prod(ex, l, rSub.fst, en, ex.location),
          rSub.snd
        )
        end
      else if r.isAvail && !availSimul(l, remain, fmts)
      then
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(
                s"t${head(remain)}${toString(idx)}",
                location=ex.location
              ),
              location=ex.location
            ),
            en,
            location=ex.location
          ),
          idx+1
        )
      else if r.isAvail
      then
        let lSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(l, var, remain, idx, fmts)
        in
        pair(
          prod(ex, lSub.fst, r, en, ex.location),
          lSub.snd
        )
        end
      else
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(
                s"t${head(remain)}${toString(idx)}",
                location=ex.location
              ),
              location=ex.location
            ),
            en,
            location=ex.location
          ),
          idx+1
        );
}

function list_reduceDeeper_function
Pair<[Pair<String TensorExpr>] Integer> ::= 
  remain::[String] idx::Integer ex::Expr
  env::Decorated Env l::TensorExpr r::TensorExpr
  expr::TensorExpr fmts::tm:Map<String TensorFormat>
  addSub::Boolean
{
  l.remaining = remain;
  l.fmts = fmts;
  r.remaining = remain;
  r.fmts = fmts;
 
  return
    if addSub
    then -- add / sub
      if anyAvail(l, remain, fmts) && anyAvail(r, remain, fmts)
      then
        let lSub::Pair<[Pair<String TensorExpr>] Integer> =
          list_reduceDeeper_helper(l, remain, idx, fmts)
        in
        let rSub::Pair<[Pair<String TensorExpr>] Integer> =
          list_reduceDeeper_helper(r, remain, lSub.snd, fmts)
        in
        pair(
          lSub.fst ++ rSub.fst,
          rSub.snd
        )
        end
        end
      else if anyAvail(l, remain, fmts)
      then
        let lSub::Pair<[Pair<String TensorExpr>] Integer> =
          list_reduceDeeper_helper(l, remain, idx, fmts)
        in
        let rSub::Pair<[Pair<String TensorExpr>] Integer> =
          pair(
            [pair(s"t${head(remain)}${toString(lSub.snd)}", r)],
            lSub.snd+1
          )
        in
        pair(
          lSub.fst ++ rSub.fst,
          rSub.snd
        )
        end
        end
      else if anyAvail(r, remain, fmts)
      then
        let lSub::Pair<[Pair<String TensorExpr>] Integer> =
          pair(
            [pair(s"t${head(remain)}${toString(idx)}", l)],
            idx+1
          )
        in
        let rSub::Pair<[Pair<String TensorExpr>] Integer> =
          list_reduceDeeper_helper(r, remain, lSub.snd, fmts)
        in
        pair(
          lSub.fst ++ rSub.fst,
          rSub.snd
        )
        end
        end
      else
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", expr)],
          idx+1
        )
    else -- mul / div
      if l.isAvail && !availSimul(r, remain, fmts)
      then
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", expr)],
          idx+1
        )
      else if l.isAvail
      then
        list_reduceDeeper_helper(r, remain, idx, fmts)
      else if r.isAvail && !availSimul(l, remain, fmts)
      then
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", expr)],
          idx+1
        )
      else if r.isAvail
      then
        list_reduceDeeper_helper(l, remain, idx, fmts)
      else
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", expr)],
          idx+1
        );
}

function availSimul
Boolean ::= 
  ex::TensorExpr remain::[String] fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return
    if ex.isAvail
    then true
    else if anyAvail(ex, remain, fmts)
    then false
    else
      availSimul(ex, tail(remain), fmts);
}

function list_reducedSubs_function
Pair<[Pair<String TensorExpr>] Integer> ::=
  remain::[String] idx::Integer ex::Expr env::Decorated Env
  l::TensorExpr r::TensorExpr exp::TensorExpr 
  fmts::tm:Map<String TensorFormat>
{
  return
    if anyAvail(l, remain, fmts) && anyAvail(r, remain, fmts)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(l, remain, idx, fmts)
      in
      let rSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(r, remain, lSub.snd, fmts)
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else if anyAvail(l, remain, fmts)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(l, remain, idx, fmts)
      in
      let rSub::Pair<[Pair<String TensorExpr>] Integer> =
        pair(
          [pair(s"t${head(remain)}${toString(lSub.snd)}", l)],
          lSub.snd+1
        )
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else if anyAvail(r, remain, fmts)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", l)],
          idx+1
        )
      in
      let rSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(r, remain, lSub.snd, fmts)
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
      else
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", exp)],
          idx+1
        );
}

function anyAvail
Boolean ::= ex::TensorExpr remain::[String] fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return 
    ex.isAvail
    ||
    case ex of
    | tensorAdd(_, l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorSub(_, l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorMul(_, l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorDiv(_, l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | _ -> false
    end;
}

function exprContained
Boolean ::= top::TensorExpr pc::TensorExpr fmts::tm:Map<String TensorFormat>
{
  return
    if exprEqual(top, pc, fmts)
    then true
    else 
      case top of
      | tensorBaseExpr(_, _) -> false
      | tensorAccess(_, _, _, _) -> false
      | tensorAdd(_, l, r, _) ->
        exprContained(l, pc, fmts) 
        ||
        exprContained(r, pc, fmts)
      | tensorSub(_, l, r, _) ->
        exprContained(l, pc, fmts)
        ||
        exprContained(r, pc, fmts)
      | tensorMul(_, l, r, _) ->
        exprContained(l, pc, fmts)
        ||
        exprContained(r, pc, fmts)
      | tensorDiv(_, l, r, _) ->
        exprContained(l, pc, fmts)
        ||
        exprContained(r, pc, fmts)
      end;
}

function exprCanZero
[TensorExpr] ::= ex::TensorExpr
{
  return
    case ex of
    | tensorBaseExpr(_, _) -> [ex]
    | tensorAccess(_, _, _, _) -> [ex]
    | tensorAdd(e, l, r, n) ->
      ex :: exprCanZero(l) ++ exprCanZero(r)
      ++
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorAdd(e, eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorSub(e, l, r, n) ->
      ex :: exprCanZero(l) ++ exprCanZero(r)
      ++
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorSub(e, eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorMul(e, l, r, n) ->
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorMul(e, eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorDiv(e, l, r, n) ->
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorDiv(e, eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    end;
}

function exprEqual
Boolean ::= a::TensorExpr b::TensorExpr fmts::tm:Map<String TensorFormat>
{
  a.fmts = fmts;
  b.fmts = fmts;

  return
    case a, b of
    | tensorBaseExpr(e1, _), tensorBaseExpr(e2, _) ->
      a.exprName == b.exprName
    | tensorAccess(_, _, _, _), tensorAccess(_, _, _, _) ->
      a.tensorName == b.tensorName
      &&
      lstEqual(head(a.accesses), head(b.accesses))
    | tensorAdd(_, l1, r1, _), tensorAdd(_, l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      &&
      exprEqual(r1, r2, fmts)
    | tensorSub(_, l1, r1, _), tensorSub(_, l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      &&
      exprEqual(r1, r2, fmts)
    | tensorMul(_, l1, r1, _), tensorMul(_, l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      && 
      exprEqual(r1, r2, fmts)
    | tensorDiv(_, l1, r1, _), tensorDiv(_, l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      &&
      exprEqual(r1, r2, fmts)
    | _, _ -> false
    end;
}

function lstEqual
Boolean ::= l1::[String] l2::[String]
{
  return
    case l1, l2 of
    | [], [] -> true
    | h1::t1, h2::t2 -> h1 == h2 && lstEqual(t1, t2)
    | _, _ -> false
    end;
}

function performSubs
TensorExpr ::= 
  ex::TensorExpr sbs::[Pair<String TensorExpr>] fmts::tm:Map<String TensorFormat>
{
  return
    foldl(
      \ e::TensorExpr sb::Pair<String TensorExpr> ->
        if exprContained(e, sb.snd, fmts)
        then 
          performSub_helper(e, sb, fmts)
        else
          e
      ,
      ex,
      sbs
    );
}

function performSub_helper
TensorExpr ::= 
  ex::TensorExpr sb::Pair<String TensorExpr> fmts::tm:Map<String TensorFormat>
{
  return
    if exprEqual(ex, sb.snd, fmts)
    then
      tensorBaseExpr(
        declRefExpr(
          name(
            sb.fst,
            location=ex.location
          ),
          location=ex.location
        ),
        ex.envr,
        location=ex.location
      )
    else
      case ex of
      | tensorBaseExpr(_, _) -> ex
      | tensorAccess(_, _, _, _) -> ex
      | tensorAdd(e, l, r, n) ->
        tensorAdd(
          e,
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorSub(e, l, r, n) ->
        tensorSub(
          e,
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorMul(e, l, r, n) ->
        tensorMul(
          e,
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorDiv(e, l, r, n) ->
        tensorDiv(
          e,
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      end;
}
