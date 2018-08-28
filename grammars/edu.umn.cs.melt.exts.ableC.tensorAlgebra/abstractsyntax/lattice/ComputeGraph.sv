grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- The compute graph is built by the TensorExprAssign
   production. The graph is build by providing it a 
   TensorExpr for the lhs and rhs, a map of tensor names
   to their formats, and a list of the access order -}

synthesized attribute conds :: [TensorCond]; -- All conditions for loops for this var
synthesized attribute exprs :: [[TensorExpr]]; -- The expressions valid inside each loop
synthesized attribute ifCnd :: [[TensorCond]]; -- The if conditions for each expression in each loop
synthesized attribute frthr :: [[ComputeGraph]]; -- The graph that describes the next step in computation
synthesized attribute asmbl :: Stmt; -- Code to assemble the output tensor
synthesized attribute compute :: Stmt; -- Compute code

-- Whether this graph is allowed to generate OpenMP parallelization
-- Determined by user, and we prevent multiple loops from being
-- parallelized
inherited attribute canPar :: Boolean;
inherited attribute thdCnt :: Maybe<Integer>;

nonterminal ComputeGraph with 
  conds, exprs, ifCnd, frthr, asmbl, compute,
  canPar, thdCnt;

{- Empty compute graph (used when we reach the end of the
   variable list -}
abstract production nullGraph
top::ComputeGraph ::= 
{
  top.conds = [];
  top.exprs = [];
  top.ifCnd = [];
  top.frthr = [];
  top.asmbl = nullStmt();
  top.compute = nullStmt();
}

{- Production for a real computeGraph. assign is the lhs, fmts is the map
   of tensor names to formats, value is the rhs, vars, is the order of 
   variables (or when called later, the remaining variables) -}
abstract production computeGraph
top::ComputeGraph ::=
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  vars::[String] loc::Location env::Decorated Env
{
  assign.fmts = fmts;

  local outDense :: Boolean =
    allDense(getTensorFormat(assign, fmts));

  -- Whether any of the remaining vars are used to access the output tensor
  local isBelowOut::Boolean =
    !containsAny(
      stringEq,
      tail(vars),
      head(assign.accesses)
    );

  -- Whether any variable (other than the first) is used to access output
  local above::Boolean =
    if null(assign.accesses)
    then false
    else
      containsBy(
        stringEq,
        last(head(assign.accesses)),
        tail(vars)
      );

  -- Build the lattice Point
  local lp::LatticePoint =
    lattice_points(assign, fmts, value, head(vars), loc, env, true);

  -- Flatten the lattice point into a list of points
  local pnts::[LatticePoint] =
    extractPoints(lp);

  -- Optimized and clenaed points. Remove unecessary points
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
        !foldl( -- Include if this has different sparse dimensions than anything before
          \ b::Boolean lat::LatticePoint ->
            b || 
            let e::Decorated TensorExpr =
              decorate c.value with {variable=head(vars); fmts=fmts;}
            in
            let ex::Decorated TensorExpr =
              decorate lat.value with {variable=head(vars); fmts=fmts;}
            in
            listEqual(
              \ p1::Pair<String Integer>
                p2::Pair<String Integer> ->
                p1.fst == p2.fst && p1.snd == p2.snd
              , 
              e.sparse, 
              ex.sparse
            )
            end
            end
          ,
          false,
          h
        )
        ||
        !containsWith( -- Include if no previous condition is 'above' it
          \ cn::TensorCond ->
            condIsAbove(cn, cond)
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
        nubBy(
          \ c1::TensorCond c2::TensorCond ->
            c1.ifCond == "1" && c2.ifCond == "1"
          ,
          map(
            \ lp::LatticePoint ->
              lp.cond
            ,
            lst
          )
        )
      ,
      sbLts
    );

  local exs::[TensorExpr] =
    map(
      \ p::LatticePoint ->
        p.value
      ,
      filtered
    );

  top.asmbl =
    if outDense || null(top.conds)
    then nullStmt()
    else
      foldl(
        \ above::Stmt nu::Stmt ->
          ableC_Stmt {
            $Stmt{above}
            $Stmt{nu}
          }
        ,
        generateAssemble(head(top.conds), head(exs), head(top.exprs), head(top.ifCnd), 
          head(top.frthr), head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), true),
        zip5(
            \ c::TensorCond ex::TensorExpr e::[TensorExpr] cd::[TensorCond] gr::[ComputeGraph] ->
              generateAssemble(
                c, ex, e, cd, gr, head(vars), fmts, listLength(top.conds) == 1, 
                assign, tail(vars), false)
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
    then nullStmt()
    else
      foldl(
        \ above::Stmt nu::Stmt ->
          ableC_Stmt {
            $Stmt{above}
            $Stmt{nu}
          }
        ,
        generateCode(head(top.conds), head(exs), head(top.exprs), head(top.ifCnd), 
          head(top.frthr), head(vars), fmts, listLength(top.conds) == 1, assign, tail(vars), 
          true, top.canPar, top.thdCnt),
        zip5(
          \ c::TensorCond ex::TensorExpr e::[TensorExpr] cd::[TensorCond] gr::[ComputeGraph] ->
            generateCode(
              c, ex, e, cd, gr, head(vars), fmts, listLength(top.conds) == 1, 
              assign, tail(vars), false, top.canPar, top.thdCnt)
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
    map(
      \ p::Pair<LatticePoint Integer> ->
        p.fst
      ,
      sortBy(
        \ p1::Pair<LatticePoint Integer> 
          p2::Pair<LatticePoint Integer> ->
          p1.snd <= p2.snd
        ,
        extractPoints_helper(p, 0)
      )
    );
}

function extractPoints_helper
[Pair<LatticePoint Integer>] ::= p::LatticePoint lvl::Integer
{
  return
    pair(p, lvl) ::
    flatMap(
      \ pnt::LatticePoint ->
        extractPoints_helper(pnt, lvl+1)
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
      | tensorAdd(l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] = 
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorSub(l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorMul(l, r, _) ->
        let sbsL::[Pair<String TensorExpr>] =
          listSubs_helper(l, var, remain, idx, fmts)
        in
        let sbsR::[Pair<String TensorExpr>] =
          listSubs_helper(r, var, remain, idx+listLength(sbsL), fmts)
        in
        sbsL ++ sbsR
        end
        end
      | tensorDiv(l, r, _) ->
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
      | tensorAdd(l, r, en) ->
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
          pair(tensorAdd(sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorSub(l, r, en) ->
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
          pair(tensorSub(sL.fst, sR.fst, en, location=e.location), sR.snd)
          end
          end
      | tensorMul(l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx, fmts)
        in let sR::Pair<TensorExpr Integer> =
          makeSubs_helper(r, var, remain, false, sL.snd, fmts)
        in
        pair(tensorMul(sL.fst, sR.fst, en, location=e.location), sR.snd)
        end
        end
      | tensorDiv(l, r, en) ->
        let sL::Pair<TensorExpr Integer> =
          makeSubs_helper(l, var, remain, false, idx, fmts)
        in let sR::Pair<TensorExpr Integer> = 
          makeSubs_helper(r, var, remain, false, sL.snd, fmts)
        in
        pair(tensorDiv(sL.fst, sR.fst, en, location=e.location), sR.snd)
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
Expr ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorBaseExpr(_, _) -> ableC_Expr{$name{e.exprName}}
    | tensorAccess(_, _, _) -> 
      ableC_Expr {
        $name{s"${e.tensorName}_data"}
          [$name{s"p${e.tensorName}${toString(listLength(head(e.accesses)))}"}]
      }
    | tensorAdd(l, r, _) -> 
      ableC_Expr {
        ( $Expr{evalExpr(l, fmts)} + $Expr{evalExpr(r, fmts)} )
      }
    | tensorSub(l, r, _) -> 
      ableC_Expr {
        ( $Expr{evalExpr(l, fmts)} - $Expr{evalExpr(r, fmts)} )
      }
    | tensorMul(l, r, _) -> 
      ableC_Expr {
        ( $Expr{evalExpr(l, fmts)} * $Expr{evalExpr(r, fmts)} )
      }
    | tensorDiv(l, r, _) -> 
      ableC_Expr {
        ( $Expr{evalExpr(l, fmts)} / $Expr{evalExpr(r, fmts)} )
      }
    end;
}

function evalOut
Expr ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorBaseExpr(ex, _) ->
      case decorate ex with {env=e.envr; returnType=nothing();} of
      | declRefExpr(nm) -> ableC_Expr{$name{nm.name}}
      | _ -> ableC_Expr{$name{"__error"}}
      end
    | tensorAccess(_, _, _) ->
      ableC_Expr {
        $name{s"${e.tensorName}_data"}
          [$name{s"p${e.tensorName}${toString(listLength(head(e.accesses)))}"}]
      }
    | _ -> ableC_Expr{$name{"__error"}}
    end;
}

function fullCondition
Expr ::= c::TensorCond e::TensorExpr var::String fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;
  e.variable = var;

  return
    case c of
    | allCond(_) ->
      if null(e.sparse)
      then c.cnd
      else
        foldl(
          \ cnd::Expr p::Pair<String Integer> ->
            ableC_Expr {
              ($Expr{cnd}) &&
              $name{s"p${p.fst}${toString(p.snd+1)}"} < 
              $name{s"${p.fst}${toString(p.snd+1)}_pos"}[$Expr{if p.snd == 0 then ableC_Expr{1} else ableC_Expr{$name{s"p${p.fst}${toString(p.snd)}"} + 1}}]
            }
          ,
          c.cnd,
          e.sparse
        )
    | denseAccess(_, _, _) ->
      if null(e.sparse)
      then c.cnd
      else
        foldl(
          \ cnd::Expr p::Pair<String Integer> ->
            ableC_Expr {
              ($Expr{cnd}) &&
              $name{s"p${p.fst}${toString(p.snd+1)}"} < 
              $name{s"${p.fst}${toString(p.snd+1)}_pos"}[$Expr{if p.snd == 0 then ableC_Expr{1} else ableC_Expr{$name{s"p${p.fst}${toString(p.snd)}"} + 1}}]
            }
          ,
          c.cnd,
          e.sparse
        )
    | _ -> c.cnd
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
Stmt ::= 
  c::TensorCond ex::TensorExpr e::[TensorExpr] ic::[TensorCond] 
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
  canEmitFor::Boolean assign::TensorExpr remain::[String]
  top::Boolean canPar::Boolean thdCnt::Maybe<Integer>
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
    canPar && forLoop && !outSparse.isJust && outDense.isJust;

  local forVar::String =
    case c of
    | allCond(v) -> v
    | denseAccess(_, _, v) -> v
    | sparseAccess(n, d, _) -> s"p${n}${toString(d+1)}"
    | _ -> ""
    end;

  local forInit::Decl =
    case c of
    | allCond(v) -> ableC_Decl{ unsigned long $name{v} = 0; }
    | denseAccess(_, _, v) -> ableC_Decl{ unsigned long $name{v} = 0; }
    | sparseAccess(n, d, _) -> 
      ableC_Decl {
        unsigned long $name{s"p${n}${toString(d+1)}"} =
          $name{s"${n}${toString(d+1)}_pos"}
            [ $Expr{if d == 0 then ableC_Expr{0} else ableC_Expr{$name{s"p${n}${toString(d)}"} } } ];
      }
    | _ -> decls(nilDecl())
    end;

  local forInitTxt::String =
    case c of
    | allCond(v) -> s"unsigned long ${v} = 0;"
    | denseAccess(_, _, v) -> s"unsigned long ${v} = 0;"
    | sparseAccess(n, d, _) ->
      s"""unsigned long ${s"p${n}${toString(d+1)}"} = ${s"${n}${toString(d+1)}_pos"}[${if d == 0 then "0" else s"p${n}${toString(d)}"}]"""
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
    | tensorAccess(_, _, _) ->
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
    | tensorAccess(_, _, _) ->
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
    | tensorAccess(_, _, _) ->
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
    | tensorAccess(_, _, _) ->
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


  local redSubs::[Pair<String TensorExpr>] =
    list_reduceDeeper(ex, v::remain, fmts);

  return
  ableC_Stmt {
    $Stmt{
      if top
      then
        foldl(
          seqStmt,
          nullStmt(),
          map(
            \ p::Pair<String Integer> ->
              if forLoop && s"p${p.fst}${toString(p.snd+1)}" == forVar
              then nullStmt()
              else
                ableC_Stmt {
                  unsigned long $name{s"p${p.fst}${toString(p.snd+1)}"} =
                    $name{s"${p.fst}${toString(p.snd+1)}_pos"}
                      [ $Expr{if p.snd == 0 then ableC_Expr { 0 } else ableC_Expr{ $name{s"p${p.fst}${toString(p.snd)}"} } } ];
                }
            ,
            ex.sparse
          )
        )
      else nullStmt()
    }
    $Stmt {
      if top
      then 
        case outSparse of
        | just(pair(s, d)) ->
          ableC_Stmt {
            unsigned long $name{s"p${s}${toString(d+1)}"} =
              $name{s"${s}${toString(d+1)}_pos"}
                [ $Expr{if d == 0 then ableC_Expr{0} else ableC_Expr{$name{s"p${s}${toString(d)}"} } } ];
          }
        | _ -> nullStmt()
        end
      else nullStmt()
    }
    $Stmt {
      if top && !forLoop && topAll
      then ableC_Stmt {
        unsigned long $name{v} = 0;
      }
      else nullStmt()
    }
    $Stmt {
      let inner::Stmt =
        ableC_Stmt {
          $Stmt {
            if listLength(ex.sparse) == 1 && (!forLoop || forVar != v) && !topAll
            then
              let p::Pair<String Integer> =
                head(ex.sparse)
              in
              ableC_Stmt {
                unsigned long $name{v} =
                  $name{s"${p.fst}${toString(p.snd+1)}_idx"}[$name{s"p${p.fst}${toString(p.snd+1)}"}];
              }
              end
            else
              ableC_Stmt {
                $Stmt{
                  foldl(
                    \ nxt::Stmt p::Pair<String Integer> ->
                      ableC_Stmt {
                        $Stmt{nxt}
                        unsigned long $name{s"${v}${p.fst}"} =
                          $name{s"${p.fst}${toString(p.snd+1)}_idx"}[$name{s"p${p.fst}${toString(p.snd+1)}"}];
                      }
                    ,
                    nullStmt(),
                    ex.sparse
                  )
                }
                $Stmt {
                  if null(ex.sparse) || (forLoop && forVar == v) || topAll
                  then nullStmt()
                  else 
                    ableC_Stmt {
                      unsigned long $name{v} = $Expr{generateMinExpr(ex.sparse, v)};
                    }
                }
              }
          }
          $Stmt{
            case outSparse of
            | just(pair(s, d)) ->
              ableC_Stmt {
                while($name{s"${s}${toString(d+1)}_idx"}[$name{s"p${s}${toString(d+1)}"}] < $name{v}) {
                  $name{s"p${s}${toString(d+1)}"}++;
                }
              }
            | nothing() -> nullStmt()
            end
          }
          $Stmt{
            foldl(
              \ inn::Stmt p::Pair<String Integer> ->
                ableC_Stmt {
                  $Stmt{inn}
                  unsigned long $name{s"p${p.fst}${toString(p.snd+1)}"} =
                    $Expr{
                      if p.snd == 0
                      then ableC_Expr{ $name{v} }
                      else
                        ableC_Expr {
                          ( $name{s"p${p.fst}${toString(p.snd)}"} *
                            $name{s"${p.fst}${toString(p.snd+1)}_size"} ) + $name{v}
                        }
                    };
                }
              ,
              nullStmt(),
              ex.dense
            )
          }
          $Stmt{
            case outDense of
            | just(pair(s, d)) ->
              ableC_Stmt {
                unsigned long $name{s"p${s}${toString(d+1)}"} =
                  $Expr{
                    if d == 0
                    then ableC_Expr{ $name{v} }
                    else
                      ableC_Expr {
                        ( $name{s"p${s}${toString(d)}"} * $name{s"${s}${toString(d+1)}_size"} ) + $name{v}
                      }
                  };
              }
            | _ -> nullStmt()
            end
          }
          $Stmt{
            if above
            then
              foldl(
                \ abv::Stmt p::Pair<String TensorExpr> ->
                  ableC_Stmt {
                    $Stmt{abv}
                    double $name{p.fst} = $Expr{evalExpr(p.snd, fmts)};
                  }
                ,
                nullStmt(),
                subs
              )
            else nullStmt()
          }
          $Stmt{
            foldr(
              \ f::(Stmt ::= Stmt) inn::Stmt ->
                f(inn)
              ,
              nullStmt(),
              zip3(
                \ c::TensorCond e::TensorExpr g::ComputeGraph ->
                  \ inn::Stmt ->
                    let red::TensorExpr =
                      reduceDeeper(e, v, remain, fmts)
                    in let sbs::[Pair<String TensorExpr>] =
                      list_reduceDeeper(e, remain, fmts)
                    in let body::Stmt =
                      ableC_Stmt {
                        $Stmt{
                          if (output || below)
                           && (decorate e with {remaining=remain; fmts=fmts;}).isAvail
                          then nullStmt()
                          else
                            ableC_Stmt {
                              $Stmt{
                                if output || below
                                then
                                  foldl(
                                    \ abv::Stmt s::Pair<String TensorExpr> ->
                                      ableC_Stmt {
                                        $Stmt{abv}
                                        double $name{s.fst} = 0.0;
                                      }
                                    ,
                                    nullStmt(),
                                    sbs
                                  )
                                else nullStmt()
                              }
                              $Stmt{(decorate g with {canPar=canPar&&!canParallel;thdCnt=thdCnt;}).compute}
                            }
                        }
                        $Stmt{
                          if below
                          then
                            foldl(
                              \ abv::Stmt p::Pair<String TensorExpr> ->
                                if exprContained(e, p.snd, fmts)
                                then
                                  let sb::TensorExpr =
                                    performSubs(p.snd, sbs, fmts)
                                  in
                                  ableC_Stmt {
                                    $name{p.fst} += $Expr{evalExpr(sb, fmts)};
                                  }
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
                                    ableC_Stmt {
                                      $name{p.fst} += $Expr{evalExpr(getElem(possible, i).fromJust, fmts)};
                                    }
                                  else nullStmt()
                                  end end
                              ,
                              nullStmt(),
                              redSubs
                            )
                          else nullStmt()
                        }
                        $Stmt{
                          if output
                          then 
                            ableC_Stmt {
                              $Expr{evalOut(assign, fmts)} += $Expr{evalExpr(red, fmts)};
                            }
                          else nullStmt()
                        }
                        $Stmt{
                          case outSparse of
                          | just(pair(s, d)) ->
                            ableC_Stmt {
                              $name{s"p${s}${toString(d+1)}"}++;
                            }
                          | _ -> nullStmt()
                          end
                        }
                      }
                    in
                    if c.ifCond == "1" || emitElse
                    then
                      body
                    else
                      ableC_Stmt {
                        if($Expr{c.ifCondition}) {
                          $Stmt{body}
                        } else {
                          $Stmt{inn}
                        }
                      }
                    end end end
                ,
                ic,
                e,
                g
              )
            )
          }
          $Stmt{
            if listLength(ex.sparse) == 1 && !topAll
            then
              let p::Pair<String Integer> =
                head(ex.sparse)
              in
              if forLoop && forVar == s"p${p.fst}${toString(p.snd+1)}"
              then nullStmt()
              else
                ableC_Stmt {
                  $name{s"p${p.fst}${toString(p.snd+1)}"}++;
                }
              end
            else
              foldl(
                \ abv::Stmt p::Pair<String Integer> ->
                  ableC_Stmt {
                    $Stmt{abv}
                    if($name{s"${v}${p.fst}"} == $name{v}) $name{s"p${p.fst}${toString(p.snd+1)}"}++;
                  }
                ,
                nullStmt(),
                ex.sparse
              )
          }
          $Stmt{
            if !forLoop && topAll
            then
              ableC_Stmt{
                $name{v}++;
              }
            else nullStmt()
          }
        }
      in
        if forLoop
        then
          if canParallel
          then 
            if thdCnt.isJust
            then ableC_Stmt {
              $Decl{forInit}
              $Stmt{txtStmt(s"#pragma omp parallel for num_threads (${toString(thdCnt.fromJust)})")}
              $Stmt{txtStmt(s"for(${forInitTxt} ${generateFullCondition(c, ex, v, fmts)}; ${forVar}++)")} {
                $Stmt{inner}
              }
            }
            else ableC_Stmt {
              $Decl{forInit}
              $Stmt{txtStmt("#pragma omp parallel for")}
              $Stmt{txtStmt(s"for(${forInitTxt} ${generateFullCondition(c, ex, v, fmts)}; ${forVar}++)")} {
                $Stmt{inner}
              }
            }
          else ableC_Stmt {
            for($Decl{forInit} $Expr{fullCondition(c, ex, v, fmts)}; $name{forVar}++) {
              $Stmt{inner}
            }
          }
        else ableC_Stmt {
          while($Expr{fullCondition(c, ex, v, fmts)}) {
            $Stmt{inner}
          }
        }
      end
    }
  };
}

function generateMinExpr
Expr ::= prs::[Pair<String Integer>] var::String
{
  return
    case prs of
    | [] -> ableC_Expr { 0 }
    | p::[] -> ableC_Expr { $name{s"${var}${p.fst}"} }
    | p::tl ->
      ableC_Expr {
        ({ unsigned min = $Expr{generateMinExpr(tl, var)};
           $name{s"${var}${p.fst}"} < min ? $name{s"${var}${p.fst}"} : min; })
      }
    end;
}

function generateAssemble
Stmt ::= 
  c::TensorCond ex::TensorExpr e::[TensorExpr] ic::[TensorCond]
  g::[ComputeGraph] v::String fmts::tm:Map<String TensorFormat>
  canEmitFor::Boolean assign::TensorExpr remain::[String]
  top::Boolean
{
  local fmtNm::String =
    getTensorFormat(assign, fmts).proceduralName;
  local oC::Integer =
    let i :: Integer =
      positionOf(
        stringEq,
        v,
        head(assign.accesses)
      )
    in
    getElem(getTensorFormat(assign, fmts).storage, i).fromJust.snd.fst
    end;

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

  local forInit::Decl =
    case c of
    | allCond(v) -> ableC_Decl { unsigned long $name{v} = 0; }
    | denseAccess(_, _, v) -> ableC_Decl { unsigned long $name{v} = 0; }
    | sparseAccess(n, d, _) -> 
      ableC_Decl {
        unsigned long $name{s"p${n}${toString(d+1)}"} = 
          $name{s"${n}${toString(d+1)}_pos"}[$Expr{if d == 0 then ableC_Expr{0} else ableC_Expr{$name{s"p${n}${toString(d)}"}}}];
      }
    | _ -> decls(nilDecl())
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
    | tensorAccess(_, _, _) ->
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
    | tensorAccess(_, _, _) ->
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
    | tensorAccess(_, _, _) ->
      !null(assign.dense ++ assign.sparse)
    | _ -> false
    end;

  assign.variable = v;
  assign.fmts = fmts;

  local topAll::Boolean =
    case c of
    | allCond(_) -> true
    | denseAccess(_, _, _) -> true
    | _ -> false
    end;


  return
  if below
  then nullStmt()
  else
  ableC_Stmt {
    $Stmt{
      if top
      then
        foldl(
          seqStmt,
          nullStmt(),
          map(
            \ p::Pair<String Integer> ->
              if forLoop && s"p${p.fst}${toString(p.snd+1)}" == forVar
              then nullStmt()
              else
                ableC_Stmt {
                  unsigned long $name{s"p${p.fst}${toString(p.snd+1)}"} =
                    $name{s"${p.fst}${toString(p.snd+1)}_pos"}
                      [ $Expr{if p.snd == 0 then ableC_Expr { 0 } else ableC_Expr { $name{s"p${p.fst}${toString(p.snd)}"} }} ];
                }
            ,
            ex.sparse
          )
        )
      else nullStmt()
    }
    $Stmt{
      if top && !forLoop && topAll
      then
        ableC_Stmt {
          unsigned long $name{v} = 0;
        }
      else nullStmt()
    }
    $Stmt{
      let inner :: Stmt =
        ableC_Stmt {
          $Stmt{
            if listLength(ex.sparse) == 1 && (!forLoop || forVar != v) && !topAll
            then
              let p::Pair<String Integer> =
                head(ex.sparse)
              in
              ableC_Stmt {
                unsigned long $name{v} = $name{s"${p.fst}${toString(p.snd+1)}_idx"}[$name{s"p${p.fst}${toString(p.snd+1)}"}];
              }
              end
            else
              ableC_Stmt {
                $Stmt{
                  foldl(
                    \ nxt::Stmt p::Pair<String Integer> ->
                      ableC_Stmt {
                        $Stmt{nxt}
                        unsigned long $name{s"${v}${p.fst}"} = $name{s"${p.fst}${toString(p.snd+1)}_idx"}[$name{s"p${p.fst}${toString(p.snd+1)}"}];
                      }
                    ,
                    nullStmt(),
                    ex.sparse
                  )
                }
                $Stmt {
                  if null(ex.sparse) || (forLoop && forVar == v) || topAll
                  then nullStmt()
                  else 
                    ableC_Stmt {
                      unsigned long $name{v} = $Expr{generateMinExpr(ex.sparse, v)};
                    }
                }
              }
          }
          $Stmt{
            if doesOut
            then
              ableC_Stmt {
                idx[$intLiteralExpr{oC}] = $name{v};
              }
            else
              nullStmt()
          }
          $Stmt{
            foldl(
              \ inn::Stmt p::Pair<String Integer> ->
                ableC_Stmt {
                  $Stmt{inn}
                  unsigned long $name{s"p${p.fst}${toString(p.snd+1)}"} =
                    $Expr{
                      if p.snd == 0
                      then ableC_Expr { $name{v} }
                      else
                        ableC_Expr {
                          ($name{s"p${p.fst}${toString(p.snd)}"} * $name{s"${p.fst}${toString(p.snd+1)}_size"}) + $name{v}
                        }
                    };
                }
              ,
              nullStmt(),
              ex.dense
            )
          }
          $Stmt{
            foldr(
              \ f::(Stmt ::= Stmt) inn::Stmt ->
                f(inn)
              ,
              nullStmt(),
              zip3(
                \ c::TensorCond e::TensorExpr g::ComputeGraph ->
                  \ inn::Stmt ->
                  let body::Stmt =
                    ableC_Stmt {
                      $Stmt{
                        if (output || below)
                          && (decorate e with {remaining=remain; fmts=fmts;}).isAvail
                        then nullStmt()
                        else g.asmbl
                      }
                      $Stmt {
                        if output
                        then
                          ableC_Stmt {
                            $name{s"tensor_getPointer_locked_${fmtNm}"}(&$name{assign.tensorName}, idx);
                          }
                        else nullStmt()
                      }
                    }
                  in
                  if c.ifCond == "1" || emitElse
                  then 
                    body
                  else
                    ableC_Stmt {
                      if($Expr{c.ifCondition})
                      {
                        $Stmt{body}
                      } else {
                        $Stmt{inn}
                      }
                    }
                  end
                ,
                ic,
                e,
                g
              )
            )
          }
          $Stmt {
            if listLength(ex.sparse) == 1 && !topAll
            then
              let p::Pair<String Integer> =
                head(ex.sparse)
              in
              if forLoop && forVar == s"p${p.fst}${toString(p.snd+1)}"
              then nullStmt()
              else 
                ableC_Stmt {
                  $name{s"p${p.fst}${toString(p.snd+1)}"}++;
                }
              end
            else
              foldl(
                \ inn::Stmt p::Pair<String Integer> ->
                  ableC_Stmt {
                    $Stmt{inn}
                    if($name{s"${v}${p.fst}"} == $name{v}) $name{s"p${p.fst}${toString(p.snd+1)}"}++;
                  }
                ,
                nullStmt(),
                ex.sparse
              )
          }
          $Stmt {
            if !forLoop && topAll
            then ableC_Stmt { $name{v}++; }
            else nullStmt()
          }
        }
      in
        if forLoop
        then
          ableC_Stmt {
            for($Decl{forInit} $Expr{fullCondition(c, ex, v, fmts)}; $name{forVar}++) {
              $Stmt{inner}
            }
          }
        else
          ableC_Stmt {
            while($Expr{fullCondition(c, ex, v, fmts)}) {
              $Stmt{inner}
            }
          }
      end
    }
  };
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
      | tensorAccess(_, _, en) ->
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
      | tensorAdd(l, r, en) ->
        reduceDeeper_function(
          tensorAdd(_, _, _, location=_), var, remain, 
          idx, en, l, r, fmts, true
        )
      | tensorSub(l, r, en) ->
        reduceDeeper_function(
          tensorSub(_, _, _, location=_), var, remain,
          idx, en, l, r, fmts, true
        )
      | tensorMul(l, r, en) ->
        reduceDeeper_function(
          tensorMul(_, _, _, location=_), var, remain, 
          idx, en, l, r, fmts, false
        )
      | tensorDiv(l, r, en) ->
        reduceDeeper_function(
          tensorDiv(_, _, _, location=_), var, remain,
          idx, en, l, r, fmts, false
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
      | tensorAccess(_, _, en) ->
        pair(
          [pair(s"t${head(remain)}${toString(idx)}", ex)],
          idx+1
        )
      | tensorAdd(l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, en, l, r, ex, fmts, true
        )
      | tensorSub(l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, en, l, r, ex, fmts, true
        )
      | tensorMul(l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, en, l, r, ex, fmts, false
        )
      | tensorDiv(l, r, en) ->
        list_reduceDeeper_function(
          remain, idx, en, l, r, ex, fmts, false
        )
      end;
}

function reduceDeeper_function
Pair<TensorExpr Integer> ::= 
  prod::(TensorExpr ::= TensorExpr TensorExpr Decorated Env Location)
  var::String remain::[String] idx::Integer en::Decorated Env
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
          prod(lSub.fst, rSub.fst, en, l.location),
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
                  location=l.location
                ),
                location=l.location
              ),
              en,
              location=l.location
            ),
            lSub.snd+1
          )
        in
        pair(
          prod(lSub.fst, rSub.fst, en, l.location),
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
                  location=l.location
                ),
                location=l.location
              ),
              en,
              location=l.location
            ),
            idx+1
          )
        in
        let rSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(r, var, remain, lSub.snd, fmts)
        in
        pair(
          prod(lSub.fst, rSub.fst, en, l.location),
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
                location=l.location
              ),
              location=l.location
            ),
            en,
            location=l.location
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
                location=l.location
              ),
              location=l.location
            ),
            en,
            location=l.location
          ),
          idx+1
        )
      else if l.isAvail
      then
        let rSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(r, var, remain, idx, fmts)
        in
        pair(
          prod(l, rSub.fst, en, l.location),
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
                location=l.location
              ),
              location=l.location
            ),
            en,
            location=l.location
          ),
          idx+1
        )
      else if r.isAvail
      then
        let lSub::Pair<TensorExpr Integer> =
          reduceDeeper_helper(l, var, remain, idx, fmts)
        in
        pair(
          prod(lSub.fst, r, en, l.location),
          lSub.snd
        )
        end
      else
        pair(
          tensorBaseExpr(
            declRefExpr(
              name(
                s"t${head(remain)}${toString(idx)}",
                location=l.location
              ),
              location=l.location
            ),
            en,
            location=l.location
          ),
          idx+1
        );
}

function list_reduceDeeper_function
Pair<[Pair<String TensorExpr>] Integer> ::= 
  remain::[String] idx::Integer
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

function anyAvail
Boolean ::= ex::TensorExpr remain::[String] fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return 
    ex.isAvail
    ||
    case ex of
    | tensorAdd(l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorSub(l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorMul(l, r, _) ->
      anyAvail(l, remain, fmts) || anyAvail(r, remain, fmts)
    | tensorDiv(l, r, _) ->
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
      | tensorAccess(_, _, _) -> false
      | tensorAdd(l, r, _) ->
        exprContained(l, pc, fmts) 
        ||
        exprContained(r, pc, fmts)
      | tensorSub(l, r, _) ->
        exprContained(l, pc, fmts)
        ||
        exprContained(r, pc, fmts)
      | tensorMul(l, r, _) ->
        exprContained(l, pc, fmts)
        ||
        exprContained(r, pc, fmts)
      | tensorDiv(l, r, _) ->
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
    | tensorAccess(_, _, _) -> [ex]
    | tensorAdd(l, r, n) ->
      ex :: exprCanZero(l) ++ exprCanZero(r)
      ++
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorAdd(eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorSub(l, r, n) ->
      ex :: exprCanZero(l) ++ exprCanZero(r)
      ++
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorSub(eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorMul(l, r, n) ->
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorMul(eL, eR, n, location=ex.location)
            ,
            exprCanZero(r)
          ),
        exprCanZero(l)
      )
    | tensorDiv(l, r, n) ->
      flatMap(
        \ eL::TensorExpr ->
          map(
            \ eR::TensorExpr ->
              tensorDiv(eL, eR, n, location=ex.location)
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
    | tensorAccess(_, _, _), tensorAccess(_, _, _) ->
      a.tensorName == b.tensorName
      &&
      lstEqual(head(a.accesses), head(b.accesses))
    | tensorAdd(l1, r1, _), tensorAdd(l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      &&
      exprEqual(r1, r2, fmts)
    | tensorSub(l1, r1, _), tensorSub(l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      &&
      exprEqual(r1, r2, fmts)
    | tensorMul(l1, r1, _), tensorMul(l2, r2, _) ->
      exprEqual(l1, l2, fmts)
      && 
      exprEqual(r1, r2, fmts)
    | tensorDiv(l1, r1, _), tensorDiv(l2, r2, _) ->
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
      | tensorAccess(_, _, _) -> ex
      | tensorAdd(l, r, n) ->
        tensorAdd(
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorSub(l, r, n) ->
        tensorSub(
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorMul(l, r, n) ->
        tensorMul(
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      | tensorDiv(l, r, n) ->
        tensorDiv(
          performSub_helper(l, sb, fmts),
          performSub_helper(r, sb, fmts),
          n,
          location=ex.location
        )
      end;
}
