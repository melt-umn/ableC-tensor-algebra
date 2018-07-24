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

  top.asmbl =
    if null(top.conds)
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
    | tensorAccess(_, t, i, _) -> e.tensorName ++ "[" ++ implode(",", head(e.accesses)) ++ "]"
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

  local redSubs :: [Pair<String TensorExpr>] =
    list_reducedSubs(ex, v::remain);

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
                reduceDeeper(e, v, remain)
              in
              let sbs::[String] =
                list_reduceDeeper(e, v, remain)
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
              && (decorate e with {remaining=remain;}).isAvail
              then ""
              else
                (
                if output || below
                then 
                  implode("\n  ",
                    map(
                      \ s::String ->
                        s"double ${s} = 0.0;"
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
                      if exprContained(e, p.snd)
                      then
                        s"${p.fst} += ${evalExpr(p.snd)};"
                      else ""
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
              s"\n  ${evalOut(assign)} += ${evalExpr(red)};"
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
    getTensorFormat(assign).proceduralName;
  local oC::Integer =
    getTensorFormat(assign).dimensions
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
    s"idx[${toString(oC-1)}] = ${v};\n"
    {-++
    s"tensor_insertBuff_mid_${fmtNm}(&(${assign.tensorName}.buffer), idx, ${toString(oC)});"-}
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
              && (decorate e with {remaining=remain;}).isAvail
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
TensorExpr ::= ex::TensorExpr var::String remain::[String]
{
  return 
    if null(remain)
    then ex
    else 
      reduceDeeper_helper(ex, var, remain, 0).fst;
}

function list_reduceDeeper
[String] ::= ex::TensorExpr var::String remain::[String]
{
  return
    if null(remain)
    then []
    else
      list_reduceDeeper_helper(ex, var, remain, 0).fst;
}

function list_reducedSubs
[Pair<String TensorExpr>] ::= ex::TensorExpr remain::[String]
{
  return
    if null(remain)
    then []
    else
      list_reducedSubs_helper(ex, remain, 0).fst;
}

function reduceDeeper_helper
Pair<TensorExpr Integer> ::= ex::TensorExpr var::String remain::[String] idx::Integer
{
  ex.remaining = remain;

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
          idx, ex, en, l, r
        )
      | tensorSub(ex, l, r, en) ->
        reduceDeeper_function(
          tensorSub(_, _, _, _, location=_), var, remain,
          idx, ex, en, l, r
        )
      | tensorMul(ex, l, r, en) ->
        reduceDeeper_function(
          tensorMul(_, _, _, _, location=_), var, remain, 
          idx, ex, en, l, r
        )
      | tensorDiv(ex, l, r, en) ->
        reduceDeeper_function(
          tensorDiv(_, _, _, _, location=_), var, remain,
          idx, ex, en, l, r
        )
      end;
}

function list_reduceDeeper_helper
Pair<[String] Integer> ::= ex::TensorExpr var::String remain::[String] idx::Integer
{
  ex.remaining = remain;

  return
    if ex.isAvail
    then pair([], idx)
    else
      case ex of
      | tensorBaseExpr(_, _) ->
        pair([], idx)
      | tensorAccess(_, _, _, en) ->
        pair(
          [s"t${head(remain)}${toString(idx)}"],
          idx+1
        )
      | tensorAdd(ex, l, r, en) ->
        list_reduceDeeper_function(
          var, remain, idx, ex, en, l, r
        )
      | tensorSub(ex, l, r, en) ->
        list_reduceDeeper_function(
          var, remain, idx, ex, en, l, r
        )
      | tensorMul(ex, l, r, en) ->
        list_reduceDeeper_function(
          var, remain, idx, ex, en, l, r
        )
      | tensorDiv(ex, l, r, en) ->
        list_reduceDeeper_function(
          var, remain, idx, ex, en, l, r
        )
      end;
}

function list_reducedSubs_helper
Pair<[Pair<String TensorExpr>] Integer> ::= expr::TensorExpr remain::[String] idx::Integer
{
  expr.remaining = remain;

  return
    if expr.isAvail
    then pair([], idx)
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
          remain, idx, ex, en, l, r, expr
        )
      | tensorSub(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr
        )
      | tensorMul(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr
        )
      | tensorDiv(ex, l, r, en) ->
        list_reducedSubs_function(
          remain, idx, ex, en, l, r, expr
        )
      end;
}

function reduceDeeper_function
Pair<TensorExpr Integer> ::= 
  prod::(TensorExpr ::= Expr TensorExpr TensorExpr Decorated Env Location)
  var::String remain::[String] idx::Integer ex::Expr en::Decorated Env
  l::TensorExpr r::TensorExpr
{
  return
    if anyAvail(l, remain) && anyAvail(r, remain)
    then
      let lSub::Pair<TensorExpr Integer> =
        reduceDeeper_helper(l, var, remain, idx)
      in
      let rSub::Pair<TensorExpr Integer> =
        reduceDeeper_helper(r, var, remain, lSub.snd)
      in
      pair(
        prod(ex, lSub.fst, rSub.fst, en, ex.location),
        rSub.snd
      )
      end
      end
    else if anyAvail(l, remain)
    then
      let lSub::Pair<TensorExpr Integer> =
        reduceDeeper_helper(l, var, remain, idx)
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
    else if anyAvail(r, remain)
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
        reduceDeeper_helper(r, var, remain, lSub.snd)
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
      );
}

function list_reduceDeeper_function
Pair<[String] Integer> ::= 
  var::String remain::[String] idx::Integer ex::Expr
  env::Decorated Env l::TensorExpr r::TensorExpr
{
  return
    if anyAvail(l, remain) && anyAvail(r, remain)
    then
      let lSub::Pair<[String] Integer> =
        list_reduceDeeper_helper(l, var, remain, idx)
      in
      let rSub::Pair<[String] Integer> =
        list_reduceDeeper_helper(r, var, remain, lSub.snd)
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else if anyAvail(l, remain)
    then
      let lSub::Pair<[String] Integer> =
        list_reduceDeeper_helper(l, var, remain, idx)
      in
      let rSub::Pair<[String] Integer> =
        pair(
          [s"t${head(remain)}${toString(lSub.snd)}"],
          lSub.snd+1
        )
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else if anyAvail(r, remain)
    then
      let lSub::Pair<[String] Integer> =
        pair(
          [s"t${head(remain)}${toString(idx)}"],
          idx+1
        )
      in
      let rSub::Pair<[String] Integer> =
        list_reduceDeeper_helper(r, var, remain, lSub.snd)
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else
      pair(
        [s"t${head(remain)}${toString(idx)}"],
        idx+1
      );
}

function list_reducedSubs_function
Pair<[Pair<String TensorExpr>] Integer> ::=
  remain::[String] idx::Integer ex::Expr env::Decorated Env
  l::TensorExpr r::TensorExpr exp::TensorExpr
{
  return
    if anyAvail(l, remain) && anyAvail(r, remain)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(l, remain, idx)
      in
      let rSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(r, remain, lSub.snd)
      in
      pair(
        lSub.fst ++ rSub.fst,
        rSub.snd
      )
      end
      end
    else if anyAvail(l, remain)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(l, remain, idx)
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
    else if anyAvail(r, remain)
    then
      let lSub::Pair<[Pair<String TensorExpr>] Integer> =
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", l)],
          idx+1
        )
      in
      let rSub::Pair<[Pair<String TensorExpr>] Integer> =
        list_reducedSubs_helper(r, remain, lSub.snd)
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
Boolean ::= ex::TensorExpr remain::[String]
{
  ex.remaining = remain;

  return 
    ex.isAvail
    ||
    case ex of
    | tensorAdd(_, l, r, _) ->
      anyAvail(l, remain) || anyAvail(r, remain)
    | tensorSub(_, l, r, _) ->
      anyAvail(l, remain) || anyAvail(r, remain)
    | tensorMul(_, l, r, _) ->
      anyAvail(l, remain) || anyAvail(r, remain)
    | tensorDiv(_, l, r, _) ->
      anyAvail(l, remain) || anyAvail(r, remain)
    | _ -> false
    end;
}

function exprContained
Boolean ::= top::TensorExpr pc::TensorExpr
{
  return
    if exprEqual(top, pc)
    then true
    else 
      case top of
      | tensorBaseExpr(_, _) -> false
      | tensorAccess(_, _, _, _) -> false
      | tensorAdd(_, l, r, _) ->
        exprContained(l, pc) 
        ||
        exprContained(r, pc)
      | tensorSub(_, l, r, _) ->
        exprContained(l, pc)
        ||
        exprContained(r, pc)
      | tensorMul(_, l, r, _) ->
        exprContained(l, pc)
        ||
        exprContained(r, pc)
      | tensorDiv(_, l, r, _) ->
        exprContained(l, pc)
        ||
        exprContained(r, pc)
      end;
}

function exprEqual
Boolean ::= a::TensorExpr b::TensorExpr
{
  return
    case a, b of
    | tensorBaseExpr(e1, _), tensorBaseExpr(e2, _) ->
      a.exprName == b.exprName
    | tensorAccess(_, _, _, _), tensorAccess(_, _, _, _) ->
      a.tensorName == b.tensorName
      &&
      lstEqual(head(a.accesses), head(b.accesses))
    | tensorAdd(_, l1, r1, _), tensorAdd(_, l2, r2, _) ->
      exprEqual(l1, l2)
      &&
      exprEqual(r1, r2)
    | tensorSub(_, l1, r1, _), tensorSub(_, l2, r2, _) ->
      exprEqual(l1, l2)
      &&
      exprEqual(r1, r2)
    | tensorMul(_, l1, r1, _), tensorMul(_, l2, r2, _) ->
      exprEqual(l1, l2)
      && 
      exprEqual(r1, r2)
    | tensorDiv(_, l1, r1, _), tensorDiv(_, l2, r2, _) ->
      exprEqual(l1, l2)
      &&
      exprEqual(r1, r2)
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
