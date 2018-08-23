grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production halideSetup
top::Stmt ::= tensor::Expr idx::Expr value::Expr inner::Stmt
{
  propagate substituted;
  top.pp = text("// Halide Tensor Expr Setup");
  top.functionDefs := [];

  local out::TensorExpr =
    tensorAccess(tensor, idx, top.env, location=tensor.location);
  local ex::TensorExpr =
    value.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors ++ out.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      \ e::TensorExpr ->
        getTensorFormat(e, tm:empty(compareString))
      ,
      tensors
    );

  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer = 
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local outNew :: TensorExpr =
    modifyNames(
      drop(
        listLength(ex.tensors),
        newNames
      ),
      out
    );

  local exNew :: TensorExpr =
    modifyNames(
      take(
        listLength(ex.tensors),
        newNames
      ),
      ex
    );

  out.fmts = fmts;
  ex.fmts = fmts;
  outNew.fmts = fmts;
  exNew.fmts = fmts;

  local access :: [String] =
    nubBy(
      stringEq, 
      concat(out.accesses ++ ex.accesses)
    );

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  local originalNames :: [String] =
    nubBy(
      stringEq,
      map(
        getTensorName(_),
        ex.tensors
      )
    );

  local tensorDecls :: [Stmt] =
    maybeMap(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(ex, _, _) ->
          case decorate ex with {env=e.envr; returnType=nothing();} of
          | declRefExpr(name(_)) -> nothing()
          | _ ->
            let fmt::TensorFormat =
              getTensorFormat(e, fmts)
            in
            let nm::String = 
              getTensorName(e)
            in
            just(
              ableC_Stmt {
                struct $name{s"tensor_${fmt.proceduralName}"} $name{nm} = $Expr{ex};
              }
            )
            end
            end
          end
        | _ -> nothing()
        end
      ,
      exNew.tensors
    );

  local exprDecls :: [Stmt] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          let nm::String =
            getExprName(e, top.env)
          in
          just(
            ableC_Stmt {
              double $name{nm} = $Expr{e};
            }
          )
          end
        end
      ,
      exNew.exprs
    );

  local requestLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_rdlock(&($name{t}.lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        pthread_rwlock_wrlock(&($name{outNew.tensorName}.lock));
      },
      originalNames
    );

  local releaseLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_unlock(&($name{t}.lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        pthread_rwlock_unlock(&($name{outNew.tensorName}.lock));
      },
      originalNames
    );

  local tensorDecl :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      tensorDecls
    );

  local exprDecl :: Stmt =
    foldl(
      \ inn::Stmt s::Stmt ->
        ableC_Stmt {
          $Stmt{s}
          $Stmt{inn}
        }
      ,
      nullStmt(),
      exprDecls
    );

  local tensorNameSub :: Stmt =
    foldl(
      \ s1::Stmt pr::Pair<String Pair<String String>> ->
        seqStmt(s1,
          if pr.snd.fst == pr.snd.snd
          then
            nullStmt()
          else
            ableC_Stmt {
              struct $name{s"tensor_${pr.fst}"} $name{pr.snd.fst} = $name{pr.snd.snd};
            }
        )
      ,
      nullStmt(),
      zipWith(
        pair,
        map(
          \ f::TensorFormat ->
            f.proceduralName
          ,
          tensorFormats
        ),
        zipWith(pair, newNames, tensorNames)
      )
    );

  local checkDims :: Stmt =
    halide_check_dims(out, exNew, access, fmts);

  local initData :: Stmt =
    foldl(
      \ s1::Stmt t::String ->
        seqStmt(
         s1,
         ableC_Stmt {
           double* $name{s"${t}_data"} = $name{t}.data;
         }
        )
      ,
      nullStmt(),
      newNames
    );

  local zeroOut :: Stmt =
    ableC_Stmt {
      memset($name{outNew.tensorName}.data, 0, $name{outNew.tensorName}.dataLen * sizeof(double));
    };

  local lErrors :: [Message] =
    checkTensorHeader(tensor.location, top.env);

  local fwrd::Stmt =
    ableC_Stmt {
    {
      $Stmt{tensorDecl}
      $Stmt{requestLocks}
      $Stmt{tensorNameSub}
      $Stmt{exprDecl}
      $Stmt{checkDims}
      $Stmt{initData}
      $Stmt{zeroOut}
      $Stmt{inner}
      $Stmt{releaseLocks}
    }
    };

  fwrd.env = top.env;
  fwrd.returnType = top.returnType;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else if !null(fwrd.errors)
    then warnStmt(fwrd.errors)
    else fwrd;
}

abstract production halideScalarTensorExpr
top::IterStmt ::= output::Name expr::Expr
{
  propagate substituted;
  top.pp = expr.pp;

  local out::TensorExpr =
    tensorBaseExpr(
      declRefExpr(
        name(
          "__out__",
          location=ex.location
        ),
        location=ex.location
      ),
      top.env,
      location=ex.location
    );

  local ex::TensorExpr =
    expr.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      \ e::TensorExpr ->
        getTensorFormat(e, tm:empty(compareString))
      ,
      tensors
    );

  local accessCalc :: [Expr] =
    map(
      \ e::TensorExpr ->
        let acc::[String] =
          head((decorate e with {fmts=fmts;}).accesses)
        in
        foldl(
          \ res::Expr v::String ->
            if v == head(acc)
            then ableC_Expr { $name{v} }
            else
              ableC_Expr {
                ( ($Expr{res}) * $name{s"${v}_dimension"} ) + $name{v}
              }
          ,
          ableC_Expr { 0 },
          acc
        )
        end
      ,
      tensors
    );

  local accesses :: tm:Map<String Expr> =
    tm:add(
      zipWith(
        pair,
        newNames,
        accessCalc
      ),
      tm:empty(compareString)
    );

  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local exNew::TensorExpr =
    modifyNames(
      newNames, 
      ex
    );
  
  out.fmts = fmts;
  ex.fmts = fmts;
  exNew.fmts = fmts;

  local order::Maybe<[String]> =
    mergeOrder(ex.accesses);

  local access::[String] =
    case order of
    | nothing() -> []
    | just(l) -> l
    end;

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        \ s::String f::TensorFormat ->
          pair(s, f)
        ,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  exNew.accessOrder = access;

  local allDense::[Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) ->
          !containsBy(
            integerEqual,
            storeSparse,
            specs
          )
        | _ -> false
        end
      ,
      tensorFormats
    );

  local localErrors :: [Message] =
    checkTensorHeader(output.location, top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else err(fmt.fst.location, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zipWith(pair, tensors, allDense)
    )
    ++
    expr.errors
    ++
    case order of
    | nothing() -> [err(expr.location, "Cannot generate code for this tensor expression due to cyclical access patterns")]
    | just(_) -> []
    end;

  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    mapWithTail(
      \ v::String rm::[String] ->
        pair(v, denseReduce(exNew, v, rm, fmts))
      ,
      access
    );

  local innerLoops :: IterStmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::IterStmt ->
        multiForIterStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(p.fst, location=expr.location),
            declRefExpr(
              name(s"${p.fst}_dimension", location=expr.location),
              location=expr.location
            ),
            nilIterVar()
          ),
          if p.snd.isJust
          then
            seqIterStmt(
              iter,
              stmtIterStmt(
                ableC_Stmt {
                  __result += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
                }
              )
            )
          else
            iter
        )
      ,
      nullIterStmt(),
      innerVars
    );

  forwards to
    if !null(localErrors)
    then stmtIterStmt(warnStmt(localErrors))
    else innerLoops;
}

abstract production halideScalarExprOrder
top::IterStmt ::= output::Name expr::Expr access::[String]
{
  propagate substituted;
  top.pp = expr.pp;

  local out::TensorExpr =
    tensorBaseExpr(
      declRefExpr(
        name(
          "__out__",
          location=ex.location
        ),
        location=ex.location
      ),
      top.env,
      location=ex.location
    );

  local ex::TensorExpr =
    expr.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors;

  local tensorNames::[String] = 
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local accessCalc :: [Expr] =
    map(
      \ e::TensorExpr ->
        let acc::[String] =
          head((decorate e with {fmts=fmts;}).accesses)
        in
        foldl(
          \ res::Expr v::String ->
            if v == head(acc)
            then ableC_Expr { $name{v} }
            else
              ableC_Expr {
                ( ($Expr{res}) * $name{s"${v}_dimension"} ) + $name{v}
              }
          ,
          ableC_Expr { 0 },
          acc
        )
        end
      ,
      tensors
    );

  local accesses :: tm:Map<String Expr> =
    tm:add(
      zipWith(
        pair,
        newNames,
        accessCalc
      ),
      tm:empty(compareString)
    );

  local newNames :: [String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local exNew :: TensorExpr =
    modifyNames(
      newNames,
      ex
    );

  out.fmts = fmts;
  ex.fmts = fmts;
  exNew.fmts = fmts;

  local allVars :: [String] =
    nubBy(
      stringEq,
      concat(exNew.accesses)
    );

  local missingVar :: Boolean =
    !containsAll(stringEq, allVars, access)
    ||
    !containsAll(stringEq, access, allVars);

  local fmts :: tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  exNew.accessOrder = access;

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) ->
          !containsBy(
            integerEqual,
            storeSparse,
            specs
          )
        | _ -> false
        end
      ,
      tensorFormats
    );

  local localErrors :: [Message] =
    checkTensorHeader(output.location, top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else err(fmt.fst.location, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zipWith(pair, tensors, allDense)
    )
    ++
    expr.errors
    ++
    if missingVar
    then [err(expr.location, s"Specified order for the loops cannot be used as some dimensions are missing.")]
    else [];

  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    mapWithTail(
      \ v::String rm::[String] ->
        pair(v, denseReduce(exNew, v, rm, fmts))
      ,
      access
    );

  local innerLoops :: IterStmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::IterStmt ->
        multiForIterStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(p.fst, location=expr.location),
            declRefExpr(
              name(s"${p.fst}_dimension", location=expr.location),
              location=expr.location
            ),
            nilIterVar()
          ),
          if p.snd.isJust
          then 
            seqIterStmt(
              iter,
              stmtIterStmt(
                ableC_Stmt {
                  __result += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
                }
              )
            )
          else
            iter
        )
      ,
      nullIterStmt(),
      innerVars
    );

  forwards to
    if !null(localErrors)
    then stmtIterStmt(warnStmt(localErrors))
    else innerLoops;
}

abstract production halideScalarSetup
top::Stmt ::= output::Name expr::Expr inner::Stmt
{
  propagate substituted;
  top.pp = text("// Halide Tensor Expr Setup");
  top.functionDefs := [];

  local ex::TensorExpr =
    expr.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local originalNames :: [String] =
    nubBy(
      stringEq,
      tensorNames
    );

  local requestLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_rdlock(&($name{t}.lock));
          $Stmt{inn}
        }
      ,
      nullStmt(),
      originalNames
    );

  local releaseLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_unlock(&($name{t}.lock));
          $Stmt{inn}
        }
      ,
      nullStmt(),
      originalNames
    );

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local exNew :: TensorExpr =
    modifyNames(
      newNames,
      ex
    );

  ex.fmts = fmts;
  exNew.fmts = fmts;

  local out::TensorExpr =
    tensorBaseExpr( 
      declRefExpr(
        name(
          "__out__",
          location=expr.location
        ),
        location=expr.location
      ),
      top.env,
      location=expr.location
    );

  local access::[String] =
    nubBy(
      stringEq,
      concat(ex.accesses)
    );

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  local tensorDecls :: [Stmt] =
    maybeMap(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(ex, _, _) ->
          case decorate ex with {env=e.envr; returnType=nothing();} of
          | declRefExpr(name(_)) -> nothing()
          | _ ->
            let fmt::TensorFormat =
              getTensorFormat(e, fmts)
            in
            let nm::String = 
              getTensorName(e)
            in
            just(
              ableC_Stmt {
                struct $name{s"tensor_${fmt.proceduralName}"} $name{nm} = $Expr{ex};
              }
            )
            end
            end
          end
        | _ -> nothing()
        end
      ,
      exNew.tensors
    );

  local exprDecls :: [Stmt] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          let nm::String =
            getExprName(e, top.env)
          in
          just(
            ableC_Stmt {
              double $name{nm} = $Expr{e};
            }
          )
          end
        end
      ,
      exNew.exprs
    );

  local tensorDecl :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      tensorDecls
    );

  local exprDecl :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      exprDecls
    );

  local tensorNameSub :: Stmt =
    foldl(
      \ s1::Stmt pr::Pair<String Pair<String String>> ->
        seqStmt(s1,
          if pr.snd.fst == pr.snd.snd
          then nullStmt()
          else
            ableC_Stmt {
              struct $name{s"tensor_${pr.fst}"} $name{pr.snd.fst} = $name{pr.snd.snd};
            }
        )
      ,
      nullStmt(),
      zipWith(
        pair,
        map(
          \ f::TensorFormat ->
            f.proceduralName
          ,
          tensorFormats
        ),
        zipWith(pair, newNames, tensorNames)
      )
    );

  local initData :: Stmt =
    foldl(
      \ s1::Stmt t::String ->
        ableC_Stmt {
          $Stmt{s1}
          double* $name{s"${t}_data"} = $name{t}.data;
        }
      ,
      nullStmt(),
      newNames
    );

  local checkDims :: Stmt =
    halide_check_dims(out, exNew, access, fmts);

  local lErrors :: [Message] =
    checkTensorHeader(output.location, top.env);

  local fwrd::Expr =
    stmtExpr(
      compoundStmt(
        seqStmt(
          tensorDecl,
          seqStmt(
            requestLocks,
            seqStmt(
              tensorNameSub,
              seqStmt(
                exprDecl,
                seqStmt(
                  checkDims,
                  seqStmt(
                    initData,
                    seqStmt(
                      inner,
                      releaseLocks
                    )
                  )
                )
              )
            )
          )
        )
      ),
      declRefExpr(
        name("__result", location=expr.location),
        location=expr.location
      ),
      location=expr.location
    );

  local finalFwrd :: Stmt =
    exprStmt(
      eqExpr(
        declRefExpr(
          output,
          location=output.location
        ),
        stmtExpr(
          ableC_Stmt {
            double __result = 0.0;
          },
          fwrd,
          location=expr.location
        ),
        location=expr.location
      )
    );

  finalFwrd.returnType = top.returnType;
  finalFwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else if !null(finalFwrd.errors)
    then warnStmt(finalFwrd.errors)
    else finalFwrd;

}

function halide_check_dims
Stmt ::= 
  out::TensorExpr ex::TensorExpr acc::[String] 
  fmts::tm:Map<String TensorFormat>
{
  local checks :: Stmt =
    foldl(
      \ inn::Stmt v::String ->
        ableC_Stmt {
          $Stmt{halide_check_var(out, ex, v, fmts)}
          $Stmt{inn}
        }
      ,
      nullStmt(),
      acc
    );

  return
    ableC_Stmt {
      char error = 0;
      $Stmt{checks}
      if(error) exit(1);
    };
}

function halide_check_var
Stmt ::=
  out::TensorExpr ex::TensorExpr var::String
  fmts::tm:Map<String TensorFormat>
{
  out.variable = var;
  ex.variable = var;
  out.fmts = fmts;
  ex.fmts = fmts;

  local acc::[Pair<String Integer>] =
    out.sparse_r ++ out.dense_r ++ ex.sparse_r ++ ex.dense_r;

  local check :: Stmt =
    let h::Pair<String Integer> =
      head(acc)
    in
    let nm::String =
      h.fst
    in
    foldl(
      \ inn::Stmt pr::Pair<String Integer> ->
        ableC_Stmt {
          if($name{nm}.dims[$intLiteralExpr{h.snd}] != $name{pr.fst}.dims[$intLiteralExpr{pr.snd}]) {
            fprintf(stderr, 
              $stringLiteralExpr{let loc::Location = out.location in s"Tensor ${nm} and ${pr.fst} do not have the same dimensionality for ${var}. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
            error = 1;
          }
        }
      ,
      nullStmt(),
      tail(acc)
    )
    end
    end;

  return
    if null(acc) 
    then nullStmt()
    else
      let h::Pair<String Integer> = 
        head(acc)
      in
      let nm::String =
        h.fst
      in
      ableC_Stmt {
        unsigned long $name{s"${var}_dimension"} = $name{nm}.dims[$intLiteralExpr{h.snd}];
        $Stmt{check}
      }
      end
      end;
}

abstract production halideTensorExpr
top::IterStmt ::= tensor::Expr idx::Expr value::Expr
{
  propagate substituted;
  top.pp = 
    ppConcat([
      tensor.pp,
      text("["),
      idx.pp,
      text("] = "),
      value.pp
    ]);
    
  local out::TensorExpr =
    tensorAccess(tensor, idx, top.env, location=tensor.location);
  local ex::TensorExpr =
    value.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors ++ out.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      \ e::TensorExpr ->
        getTensorFormat(e, tm:empty(compareString))
      ,
      tensors
    );

  local accessCalc :: [Expr] =
    map(
      \ e::TensorExpr ->
        let acc::[String] =
          head((decorate e with {fmts=fmts;}).accesses)
        in
        foldl(
          \ res::Expr v::String ->
            if v == head(acc)
            then ableC_Expr { $name{v} }
            else 
              ableC_Expr {
                ( ($Expr{res}) * $name{s"${v}_dimension"} ) + $name{v}
              }
          ,
          ableC_Expr { 0 },
          acc
        )
        end
      ,
      tensors
    );

  local accesses :: tm:Map<String Expr> =
    tm:add(
      zipWith(
        pair,
        newNames,
        accessCalc
      ),
      tm:empty(compareString)
    );

  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local outNew::TensorExpr =
    modifyNames(
      drop(
        listLength(ex.tensors),
        newNames
      ),
      out
    );

  local exNew::TensorExpr =
    modifyNames(
      take(
        listLength(ex.tensors),
        newNames
      ),
      ex
    );

  local leftOnly::[String] =
    let lAcc::[String] =
      nubBy(
        stringEq,
        concat(outNew.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        concat(exNew.accesses)
      )
    in
    filter(
      \ v::String -> !containsBy(stringEq, v, rAcc)
      ,
      lAcc
    )
    end
    end;

  local invalidLeftVar::Boolean =
    !null(leftOnly);

  out.fmts = fmts;
  ex.fmts = fmts;
  outNew.fmts = fmts;
  exNew.fmts = fmts;

  local order::Maybe<[String]> =
    mergeOrder(out.accesses ++ ex.accesses);

  local access::[String] =
    case order of
    | nothing() -> []
    | just(l) -> l
    end;

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        \ s::String f::TensorFormat ->
          pair(s, f)
        ,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  exNew.accessOrder = access;

  local allDense::[Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) ->
          !containsBy(
            integerEqual,
            storeSparse,
            specs
          )
        | _ -> false
        end
      ,
      tensorFormats
    );

  local localErrors::[Message] =
    checkTensorHeader(tensor.location, top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else err(fmt.fst.location, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zipWith(pair, tensors, allDense)
    )
    ++
    tensor.errors
    ++
    idx.errors
    ++
    value.errors
    ++
    if invalidLeftVar
    then [err(tensor.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(", ", leftOnly)} only occur on the left-hand side.")]
    else []
    ++
    case order of
    | nothing() -> [err(tensor.location, s"Cannot generate code for this tensor expression due to cyclical access patterns")]
    | just(_) -> []
    end;

  local topVars :: [String] =
    let i::Integer =
      positionOf(
        stringEq,
        last(head(out.accesses)),
        access
      )
    in
    take(i+1, access)
    end;

  local topExpr :: Maybe<TensorExpr> =
    let i::Integer =
      positionOf(
        stringEq,
        last(head(out.accesses)),
        access
      )
    in
    denseReduce(
      exNew,
      last(head(out.accesses)),
      drop(i+1, access),
      fmts
    )
    end;

  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    let i::Integer = 
      positionOf(
        stringEq,
        last(head(out.accesses)),
        access
      )
    in
    mapWithTail(
      \ v::String rm::[String] ->
        pair(v, denseReduce(exNew, v, rm, fmts))
      ,
      drop(i+1, access)
    )
    end;

  local topLoop :: IterVars =
    foldr(
      \ s::String var::IterVars ->
        consIterVar(
          builtinTypeExpr(
            nilQualifier(),
            unsignedType(
              longType()
            )
          ),
          baseTypeExpr(),
          name(s, location=value.location),
          declRefExpr(
            name(s"${s}_dimension", location=value.location),
            location=value.location
          ),
          var
        )
      ,
      nilIterVar(),
      topVars
    );

  local innerLoops :: IterStmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::IterStmt ->
        multiForIterStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(p.fst, location=value.location),
            declRefExpr(
              name(s"${p.fst}_dimension", location=value.location),
              location=value.location
            ),
            nilIterVar()
          ),
          if p.snd.isJust
          then
            seqIterStmt(
              iter,
              stmtIterStmt(
                ableC_Stmt {
                  $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
                }
              )
            )
          else
            iter
        )
      ,
      nullIterStmt(),
      innerVars
    );

  local outAcc::Expr = 
    getElem(
      accessCalc,
      positionOf(
        stringEq,
        outNew.tensorName,
        newNames
      )
    ).fromJust;

  local fwrd :: IterStmt =
    multiForIterStmt(
      topLoop
      ,
      seqIterStmt(
        innerLoops,
        if topExpr.isJust
        then
          stmtIterStmt(
            ableC_Stmt {
              $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(topExpr.fromJust, fmts, accesses)};
            }
          )
        else
          nullIterStmt()
      )
    );

  forwards to
    if !null(localErrors)
    then stmtIterStmt(warnStmt(localErrors))
    else fwrd;
}

abstract production halideTensorExprOrder
top::IterStmt ::= tensor::Expr idx::Expr value::Expr access::[String]
{
  propagate substituted;
  top.pp =
    ppConcat([
      tensor.pp,
      text("["),
      idx.pp,
      text("] = "),
      value.pp
    ]);

  local out::TensorExpr =
    tensorAccess(tensor, idx, top.env, location=tensor.location);
  local ex::TensorExpr =
    value.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors ++ out.tensors;

  local tensorNames :: [String] =
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local accessCalc :: [Expr] =
    map(
      \ e::TensorExpr ->
        let acc::[String] =
          head((decorate e with {fmts=fmts;}).accesses)
        in
        foldl(
          \ res::Expr v::String ->
            if v == head(acc)
            then ableC_Expr { $name{v} }
            else
              ableC_Expr {
                ( ($Expr{res}) * $name{s"${v}_dimension"} ) + $name{v}
              }
          ,
          ableC_Expr { 0 },
          acc
        )
        end
      ,
      tensors
    );

  local accesses :: tm:Map<String Expr> =
    tm:add(
      zipWith(
        pair,
        newNames,
        accessCalc
      ),
      tm:empty(compareString)
    );

  local newNames :: [String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(stringEq, n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  local outNew :: TensorExpr =
    modifyNames(
      drop(
        listLength(ex.tensors),
        newNames
      ),
      out
    );

  local exNew :: TensorExpr =
    modifyNames(
      take(
        listLength(ex.tensors),
        newNames
      ),
      ex
    );

  local leftOnly :: [String] =
    let lAcc::[String] = 
      nubBy(
        stringEq,
        concat(outNew.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        concat(exNew.accesses)
      )
    in
    filter(
      \ v::String -> !containsBy(stringEq, v, rAcc)
      ,
      lAcc
    )
    end
    end;

  local invalidLeftVar::Boolean =
    !null(leftOnly);

  out.fmts = fmts;
  ex.fmts = fmts;
  outNew.fmts = fmts;
  exNew.fmts = fmts;

  local allVars :: [String] =
    nubBy(
      stringEq,
      concat(outNew.accesses ++ exNew.accesses)
    );

  local missingVar :: Boolean =
    !containsAll(stringEq, allVars, access)
    ||
    !containsAll(stringEq, access, allVars);

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty(compareString)
    );

  exNew.accessOrder = access;

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) ->
          !containsBy(
            integerEqual,
            storeSparse,
            specs
          )
        | _ -> false
        end
      ,
      tensorFormats
    );

  local localErrors :: [Message] =
    checkTensorHeader(tensor.location, top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else err(fmt.fst.location, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zipWith(pair, tensors, allDense)
    )
    ++
    tensor.errors
    ++
    idx.errors
    ++
    value.errors
    ++
    if invalidLeftVar
    then [err(tensor.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(", ", leftOnly)} only occur on the left-hand side.")]
    else []
    ++
    if missingVar
    then [err(tensor.location, s"Specified order for the loops cannot be used, as some dimensions are missing.")]
    else [];

  local topVars :: [String] =
   let i::Integer =
     lastIndexOf(
       stringEq,
       head(out.accesses),
       access
      )
    in
    take(i+1, access)
    end;

  local topExpr :: Maybe<TensorExpr> =
    let i::Integer =
      lastIndexOf(
        stringEq,
        head(out.accesses),
        access
      )
    in
    denseReduce(
      exNew,
      getElem(access, i).fromJust,
      drop(i+1, access),
      fmts
    )
    end;

  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    let i::Integer =
      lastIndexOf(
        stringEq,
        head(out.accesses),
        access
      )
    in
    mapWithTail(
      \ v::String rm::[String] ->
        pair(v, denseReduce(exNew, v, rm, fmts))
      ,
      drop(i+1, access)
    )
    end;

  local topLoop :: IterVars =
    foldr(
      \ s::String var::IterVars ->
        consIterVar(
          builtinTypeExpr(
            nilQualifier(),
            unsignedType(
              longType()
            )
          ),
          baseTypeExpr(),
          name(s, location=value.location),
          declRefExpr(
            name(s"${s}_dimension", location=value.location),
            location=value.location
          ),
          var
        )
      ,
      nilIterVar(),
      topVars
    );

  local innerLoops :: IterStmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::IterStmt ->
        multiForIterStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(p.fst, location=value.location),
            declRefExpr(
              name(s"${p.fst}_dimension", location=value.location),
              location=value.location
            ),
            nilIterVar()
          ),
          if p.snd.isJust
          then
            seqIterStmt(
              iter,
              stmtIterStmt(
                ableC_Stmt {
                  $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
                }
              )
            )
          else
            iter
        )
      ,
      nullIterStmt(),
      innerVars
    );

  local outAcc::Expr =
    getElem(
      accessCalc,
      positionOf(
        stringEq,
        outNew.tensorName,
        newNames
      )
    ).fromJust;

  local fwrd::IterStmt =
    multiForIterStmt(
      topLoop,
      seqIterStmt(
        innerLoops,
        if topExpr.isJust
        then
          stmtIterStmt(
            ableC_Stmt {
              $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(topExpr.fromJust, fmts, accesses)};
            }
          )
        else
          nullIterStmt()
      )
    );

  forwards to
    if !null(localErrors)
    then stmtIterStmt(warnStmt(localErrors))
    else fwrd;
}

function denseReduce
Maybe<TensorExpr> ::= 
  ex::TensorExpr var::String remain::[String] 
  fmts::tm:Map<String TensorFormat>
{
  ex.remaining = remain;
  ex.fmts = fmts;

  return
    if ex.isAvail
    then just(ex)
    else
      case ex of
      | tensorBaseExpr(_, _) ->
        just(ex)
      | tensorAccess( _, _, en) ->
        nothing()
      | tensorAdd(l, r, en) ->
        if l.isAvail
        then just(l)
        else if r.isAvail
        then just(r)
        else nothing()
      | tensorSub(l, r, en) ->
        if l.isAvail
        then just(l)
        else if r.isAvail
        then
          just(
            tensorSub(
              nullTensorExpr(en, location=ex.location),
              r,
              en,
              location=ex.location
            )
          )
        else nothing()
      | tensorMul(l, r, en) ->
        nothing()
      | tensorDiv(l, r, en) ->
        nothing()
      end;
}

function denseEvalExpr
Expr ::=
  e::TensorExpr fmts::tm:Map<String TensorFormat>
  acc::tm:Map<String Expr>
{
  return
    case e of
    | tensorBaseExpr(_, _) ->
      ableC_Expr {
        $name{e.exprName}
      }
    | tensorAccess(_, _, _) ->
      ableC_Expr {
        $name{s"${e.tensorName}_data"}[$Expr{head(tm:lookup(e.tensorName, acc))}]
      }
    | tensorAdd(l, r, _) ->
      ableC_Expr {
        ( $Expr{denseEvalExpr(l, fmts, acc)} + $Expr{denseEvalExpr(r, fmts, acc)} )
      }
    | tensorSub(l, r, _) ->
      ableC_Expr {
        ( $Expr{denseEvalExpr(l, fmts, acc)} - $Expr{denseEvalExpr(r, fmts, acc)} )
      }
    | tensorMul(l, r, _) ->
      ableC_Expr {
        ( $Expr{denseEvalExpr(l, fmts, acc)} * $Expr{denseEvalExpr(r, fmts, acc)} )
      }
    | tensorDiv(l, r, _) ->
      ableC_Expr {
        ( $Expr{denseEvalExpr(l, fmts, acc)} / $Expr{denseEvalExpr(r, fmts, acc)} )
      }
    end;
}
