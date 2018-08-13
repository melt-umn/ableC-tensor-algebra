grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorAssignToTensor
top::Expr ::= tensor::Expr idx::Expr right::Expr
{
  propagate substituted;

  top.pp = 
    ppConcat([
      tensor.pp,
      text("["),
      idx.pp,
      text("] = "),
      right.pp
    ]);
  
  local out::TensorExpr =
    tensorAccess(tensor, tensor, idx, top.env, location=top.location);
  
  local ex::TensorExpr =
    right.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors ++ out.tensors;

  local tensorNames::[String] =
    map(
      getTensorName(_),
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
        flatMap(\l::[String] -> l, outNew.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        flatMap(\l::[String] -> l, exNew.accesses)
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
      )
      ,
      tm:empty(compareString)
    );

  exNew.accessOrder = access;

  local lErrors::[Message] =
    if invalidLeftVar
    then [err(top.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(", ", leftOnly)} only occur on the left-hand side.")]
    else []
    ++
    case order of
    | nothing() -> [err(top.location, "Cannot generate code for this tensor expression due to cyclical access patterns")]
    | just(_) -> []
    end;

  local graph::ComputeGraph =
    computeGraph(
      outNew, fmts, exNew, access,
      top.location, top.env
    );

  local maybeVar :: Maybe<String> =
    let parts::[String] =
      explode("__parallel_emit", graph.compute)
    in
    if null(tail(parts))
    then nothing()
    else
      let str::String = head(tail(parts)) in
      let start::Integer = indexOf("for", str) in
      let stop::Integer = indexOf("{", str) in
      let loop::String = substring(start, stop, str) in
        just(
          substring(
            18,
            indexOf("=", loop) - 1,
            loop
          )
        )
      end end end end
    end;

  local isParallel :: Boolean =
    case lookupValue(emitParallel, top.env) of
    | [] -> false
    | _::_ ->
      maybeVar.isJust
      &&
      let vars::[String] =
        take(
          positionOf(
            stringEq, 
            maybeVar.fromJust,
            access
          ),
          access
        )
      in
      foldl(
        \ b::Boolean p::Pair<String String> ->
          b && p.fst == p.snd
        ,
        true,
        zipWith(pair, head(outNew.accesses), vars)
      )
      end
    end;

  local parallelEmit :: Stmt =
    txtStmt(
      if !isParallel
      then ""
      else
      case lookupValue(emitParallel, top.env) of
      | [] -> ""
      | _::_ ->
        case lookupValue(emitThreads, top.env) of
        | [] -> "#pragma omp parallel for"
        | v::_ ->
          case v of
          | declaratorValueItem(
              declarator(_, _, _,
                justInitializer(
                  exprInitializer(
                    realConstant(
                      integerConstant(n, _, _)
                    )
                  )
                )
              )
            ) -> s"#pragma omp parallel for num_threads(${n})"
          | _ -> "#pragma omp parallel for"
          end
        end
      end
    );

  {- Quick testing shows that having multiple #pragma directives
     results in a slower execution time than just one #pragma,
     so, we we explode and implode to remove all but the first
     parallelization directive. -}
  local graphCode :: String =
    let lst::[String] =
      explode("__parallel_emit", graph.compute)
    in
    if null(tail(lst))
    then head(lst)
    else 
    let res::String =
      head(lst) ++ "__parallel_emit" ++
      implode(
        "",
        tail(lst)
      )
    in
    if forLoop.isJust
    then substitute(forLoop.fromJust, "__for_loop;", res)
    else res
    end
    end;

  local forLoop :: Maybe<String> =
    let lst::[String] =
      explode("__parallel_emit", graph.compute)
    in
    if null(tail(lst)) || !isParallel
    then nothing()
    else
      just(
        let str::String =
          head(tail(lst))
        in
        let start::Integer =
          indexOf("for", str)
        in
        let stop::Integer =
          indexOf("{", str)
        in
        substring(start, stop, str)
        end
        end
        end
      )
    end;

  local forVar :: Maybe<String> =
    if forLoop.isJust
    then
      just(
        let stop::Integer =
          indexOf("=", forLoop.fromJust)
        in
        substring(18, stop-1, forLoop.fromJust)
        end
      )
    else nothing();

  local forStmt :: Maybe<Stmt> =
    if forLoop.isJust
    then 
      just(
        txtStmt(forLoop.fromJust)
      )
    else nothing();

  local computeStmt::Stmt =
    substStmt(
      stmtSubstitution("__parallel_emit", parallelEmit) ::
      if forStmt.isJust
      then [stmtSubstitution("__for_loop", forStmt.fromJust)]
      else [],
      parseStmt(
        (
        if forVar.isJust
        then s"unsigned long ${forVar.fromJust};"
        else ""
        )
        ++
        graphCode
      )
    );

  local fmtNm::String =
    getTensorFormat(outNew, fmts).proceduralName;

  local outOrder::Integer = 
    getTensorFormat(outNew, fmts).dimensions;

  exNew.fmts = fmts;
  outNew.fmts = fmts;

  local lockOut :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
      ableC_Stmt {
        pthread_rwlock_wrlock(&($name{outNew.tensorName}.lock));
        $Stmt{inner}
        pthread_rwlock_unlock(&($name{outNew.tensorName}.lock));
      };

  local exprs :: [Pair<String Expr>] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          just(pair(getExprName(e, top.env), e))
        end
      ,
      exNew.exprs
    );

  local declExpr :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt p::Pair<String Expr> ->
        ableC_Stmt {
          double $name{p.fst} = $Expr{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      exprs
    );

  local preSubs :: [Pair<Pair<String String> Expr>] = -- (newName, fmt), expr
    maybeMap(
      \ t::TensorExpr ->
        case t of
        | tensorAccess(_, ex, _, _) ->
          case decorate ex with{env=top.env; returnType=nothing();} of
          | declRefExpr(name(_)) -> nothing()
          | _ ->
            just(
              pair(
                pair(
                  getTensorName(t),
                  getTensorFormat(t, tm:empty(compareString)).proceduralName
                ),
                ex
              )
            )
          end
        | _ -> nothing()
        end
      ,
      ex.tensors
    );

  local declTensor :: (Stmt ::= Stmt) =
    foldl(
      \ inn :: Stmt p::Pair<Pair<String String> Expr> ->
        ableC_Stmt {
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = $Expr{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      preSubs
    );

  local originalNames :: [Pair<String String>] =
    nubBy(
      \ p1::Pair<String String> p2::Pair<String String> ->
        p1.fst == p2.fst
      ,
      map(
        \ t::TensorExpr ->
          pair(getTensorName(t), getTensorFormat(t, tm:empty(compareString)).proceduralName)
        ,
        ex.tensors
      )
    );

  local packTensors :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::Pair<String String> ->
        ableC_Stmt {
          $name{s"tensor_pack_${t.snd}"}(&$name{t.fst});
          pthread_rwlock_rdlock(&($name{t.fst}.lock));
          $Stmt{inn}
        }
      ,
      _,
      originalNames
    );

  local postSubs :: [Pair<Pair<String String> String>] = -- (newName, fmt) oldName
    maybeMapWithTail(
      \ n ::Pair<String TensorFormat> o :: [Pair<String TensorFormat>] ->
        let c :: Integer =
          count(stringEq, n.fst, map(\p::Pair<String TensorFormat>->p.fst, o))
        in
        if c > 0
        then
          just(
            pair(
              pair(n.fst ++ toString(c) ++ "_", n.snd.proceduralName),
              n.fst
            )
          )
        else nothing()
        end
      ,
      map(
        \ t::TensorExpr ->
          pair(getTensorName(t), getTensorFormat(t, tm:empty(compareString)))
        ,
        ex.tensors
      )
    );

  local tensorSub :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt p::Pair<Pair<String String> String> ->
        ableC_Stmt {
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = $name{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      postSubs
    );

  local tensorPrep :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::TensorExpr ->
        let nm::String = getTensorName(t) in
        let indexSetup :: Stmt =
          foldl(
            \ inn::Stmt dm::Pair<Integer Pair<Integer Integer>> ->
              if dm.snd.snd == storeDense
              then
                ableC_Stmt {
                  unsigned long $name{s"${nm}${toString(dm.fst+1)}_size"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][0][0];
                  $Stmt{inn}
                }
              else
                ableC_Stmt {
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_pos"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][0];
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_idx"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][1];
                  $Stmt{inn}
                }
            ,
            nullStmt(),
            getTensorFormat(t, fmts).storage
          )
        in
        ableC_Stmt {
          double* $name{s"${nm}_data"} = $name{nm}.data;
          $Stmt{indexSetup}
          $Stmt{inn}
        }
        end end
      ,
      _,
      exNew.tensors
    );

  local dimsCheck :: (Stmt ::= Stmt) =
    let varChecks :: Stmt =
      foldl(
        \ inn::Stmt var::String ->
          ableC_Stmt {
            $Stmt{checkVar(outNew, exNew, var, fmts)}
            $Stmt{inn}
          }
        ,
        nullStmt(),
        access
      )
    in
    \ inner::Stmt ->
    ableC_Stmt {
      char error = 0;
      $Stmt{varChecks}
      if(error) exit(1);
      $Stmt{inner}
    }
    end;

  local assembleOut :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
      if allDense(getTensorFormat(outNew, fmts))
      then
        ableC_Stmt {
          memset($name{outNew.tensorName}.data, 0, $name{outNew.tensorName}.dataLen * sizeof(double));
          $Stmt{inner}
        }
      else
        ableC_Stmt {
          unsigned long idx[$intLiteralExpr{listLength(head(out.accesses))}];
          struct $name{s"tensor_${fmtNm}"}* t = &$name{outNew.tensorName};
          unsigned long count = 1;
          $Stmt{parseStmt(graph.asmbl)}

          unsigned long* dims = $name{outNew.tensorName}.dims;
          $name{s"tensor_packTree_${fmtNm}"}(&($name{outNew.tensorName}.buffer), dims);

          struct tensor_tree_s* buffer = &($name{outNew.tensorName}.buffer);

          if(t->indices) { $Stmt{parseStmt(freeIndices_String(getTensorFormat(outNew, fmts)))} }
          t->indices = calloc($intLiteralExpr{outOrder}, sizeof(unsigned long**));

          unsigned long numChildren = 1;
          struct tensor_tree_s** trees = &buffer;

          struct tensor_tree_s** temp_tree;
          unsigned long total, dimSize, index, newChildren;

          $Stmt{parseStmt(generatePackBody_Assemble(getTensorFormat(outNew, fmts).storage))}
          
          if(t->data) free(t->data);
          t->data = calloc(numChildren, sizeof(double));
          for(unsigned long i = 0; i < numChildren; i++) {
            t->data[i] = trees[i]->val;
          }

          t->dataLen = numChildren;
          t->bufferCnt = 0;
          t->buffer.numChildren = 0;
          t->buffer.children = 0;
          t->form = "";

          $Stmt{inner}
        };

  local outVal :: (Stmt ::= Stmt) =
    let nm :: String =
      getTensorName(head(outNew.tensors))
    in
    let setupOut :: Stmt =
      foldl(
        \ inn::Stmt p::Pair<Integer Pair<Integer Integer>> ->
          if p.snd.snd == storeDense
          then
            ableC_Stmt {
              unsigned long $name{s"${nm}${toString(p.fst+1)}_size"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0][0];
              $Stmt{inn}
            }
          else
            ableC_Stmt {
              unsigned long* $name{s"${nm}${toString(p.fst+1)}_pos"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0];
              unsigned long* $name{s"${nm}${toString(p.fst+1)}_idx"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][1];
              $Stmt{inn}
            }
        ,
        nullStmt(),
        getTensorFormat(head(outNew.tensors), fmts).storage
      )
    in
    \ inner::Stmt ->
    ableC_Stmt {
      double* $name{s"${nm}_data"} = $name{nm}.data;
      $Stmt{setupOut}
      $Stmt{inner}
    }
    end end;

  local comp :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
    ableC_Stmt {
    { // Extra set of braces needed to avoid redeclaration from assemble
      $Stmt{computeStmt}
      $Stmt{inner}
    }
    };

  local setFormat :: Stmt =
    foldl(
      \ inn::Stmt nm::String ->
        ableC_Stmt {
          $name{nm}.form = $stringLiteralExpr{exprToString(exNew, fmts)};
          pthread_rwlock_unlock(&($name{nm}.lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        $name{getTensorName(head(outNew.tensors))}.form = $stringLiteralExpr{exprToString(exNew, fmts)};
      },
      map(\p::Pair<String String> -> p.fst, originalNames)
    );

  forwards to
    mkErrorCheck(
      lErrors,
      stmtExpr(
        lockOut(declTensor(packTensors(declExpr(tensorSub(tensorPrep(dimsCheck(
          assembleOut(outVal(comp(setFormat)))))))))),
        ableC_Expr {
          $name{outNew.tensorName}
        },
        location=top.location
      )
    );
}

abstract production tensorAssignToScalar
top::Expr ::= output::Expr expr::Expr
{
  propagate substituted;

  top.pp = 
    ppConcat([
      output.pp,
      text(" = "),
      expr.pp
    ]);

  local out::TensorExpr =
    tensorBaseExpr(
      declRefExpr(
        name(
          "__out__",
          location=top.location
        ),
        location=top.location
      ), 
      top.env,
      location=top.location);

  local ex::TensorExpr =
    expr.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors;

  local tensorNames::[String] =
    map(
      getTensorName(_),
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

  local exNew::TensorExpr =
    modifyNames(
      newNames,
      ex
    );

  out.fmts = fmts;
  ex.fmts = fmts;
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

  local lErrors::[Message] =
    case order of
    | nothing() -> [err(top.location, "Cannot generate code for this tensor expression due to cyclical access patterns")]
    | just(_) -> []
    end;

  local graph::ComputeGraph =
    computeGraph(
      out, fmts, exNew, access,
      top.location, top.env
    );

  local isParallel :: Boolean =
    case lookupValue(emitParallel, top.env) of
    | [] -> false
    | _::_ -> false
    end;
  
  local parallelEmit :: Stmt =
    txtStmt(
      if !isParallel
      then ""
      else
      case lookupValue(emitParallel, top.env) of
      | [] -> ""
      | _::_ ->
        case lookupValue(emitThreads, top.env) of
        | [] -> "#pragma omp parallel for"
        | v::_ ->
          case v of
          | declaratorValueItem(
              declarator(_, _, _,
                justInitializer(
                  exprInitializer(
                    realConstant(
                      integerConstant(n, _, _)
                    )
                  )
                )
              )
            ) -> s"#pragma omp parallel for num_threads(${n})"
          | _ -> "#pragma omp parallel for"
          end
        end
      end
    );

  local graphCode :: String =
    let lst::[String] = 
      explode("__parallel_emit", graph.compute)
    in
    if null(tail(lst))
    then head(lst)
    else if !isParallel
    then implode("", lst)
    else
    let res::String =
      head(lst) ++ "__parallel_emit" ++
      implode(
        "",
        tail(lst)
      )
    in
    if forLoop.isJust
    then substitute(forLoop.fromJust, "__for_loop;", res)
    else res
    end
    end;

  local forLoop :: Maybe<String> =
    let lst::[String] =
      explode("__parallel_emit", graph.compute)
    in
    if null(tail(lst)) || !isParallel
    then nothing()
    else
      just(
        let str::String =
          head(tail(lst))
        in
        let start::Integer =
          indexOf("for", str)
        in
        let stop::Integer =
          indexOf("{", str)
        in
        substring(start, stop, str)
        end
        end
        end
      )
    end;

  local forVar :: Maybe<String> =
    if forLoop.isJust
    then
      just(
        let stop::Integer =
          indexOf("=", forLoop.fromJust)
        in
        substring(18, stop-1, forLoop.fromJust)
        end
      )
    else nothing();

  local forStmt :: Maybe<Stmt> =
    if forLoop.isJust
    then
      just(
        txtStmt(forLoop.fromJust)
      )
    else nothing();

  local computeStmt::Stmt =
    substStmt(
      stmtSubstitution("__parallel_emit", parallelEmit) ::
      if forStmt.isJust
      then [stmtSubstitution("__for_loop", forStmt.fromJust)]
      else [],
      parseStmt(
        (
        if forVar.isJust
        then s"unsigned long ${forVar.fromJust};\n"
        else ""
        )
        ++
        graphCode
      )
    );

  local exprs :: [Pair<String Expr>] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          just(pair(getExprName(e, top.env), e))
        end
      ,
      exNew.exprs
    );

  local declExpr :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt p::Pair<String Expr> ->
        ableC_Stmt {
          double $name{p.fst} = $Expr{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      exprs
    );

  local preSubs :: [Pair<Pair<String String> Expr>] =
    maybeMap(
      \ t::TensorExpr ->
        case t of
        | tensorAccess(_, ex, _, _) ->
          case decorate ex with {env=top.env; returnType=nothing();} of
          | declRefExpr(name(_)) -> nothing()
          | _ ->
            just(
              pair(
                pair(
                  getTensorName(t),
                  getTensorFormat(t, tm:empty(compareString)).proceduralName
                ),
                ex
              )
            )
          end
        | _ -> nothing()
        end
      ,
      ex.tensors
    );

  local declTensor :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt p::Pair<Pair<String String> Expr> ->
        ableC_Stmt {
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = $Expr{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      preSubs
    );

  local originalNames :: [Pair<String String>] =
    nubBy(
      \ p1::Pair<String String> p2::Pair<String String> ->
        p1.fst == p2.fst
      ,
      map(
        \ t::TensorExpr ->
          pair(getTensorName(t), getTensorFormat(t, tm:empty(compareString)).proceduralName)
        ,
        ex.tensors
      )
    );

  local packTensors :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::Pair<String String> ->
        ableC_Stmt {
          $name{s"tensor_pack_${t.snd}"}(&$name{t.fst});
          pthread_rwlock_rdlock(&($name{t.fst}.lock));
          $Stmt{inn}
        }
      ,
      _,
      originalNames
    );

  local postSubs :: [Pair<Pair<String String> String>] = -- (newName, fmt), oldName
    maybeMapWithTail(
      \ n::Pair<String TensorFormat> o::[Pair<String TensorFormat>] ->
        let c::Integer =
          count(stringEq, n.fst, map(\p::Pair<String TensorFormat>->p.fst, o))
        in
        if c > 0
        then
          just(
            pair(
              pair(n.fst ++ toString(c) ++ "_", n.snd.proceduralName),
              n.fst
            )
          )
        else nothing()
        end
      ,
      map(
        \ t::TensorExpr ->
          pair(getTensorName(t), getTensorFormat(t, tm:empty(compareString)))
        ,
        ex.tensors
      )
    );

  local tensorSub :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt p::Pair<Pair<String String> String> ->
        ableC_Stmt {
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = $name{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      postSubs
    );

  local tensorPrep :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::TensorExpr ->
        let nm::String = getTensorName(t) in
        let indexSetup :: Stmt =
          foldl(
            \ inn::Stmt dm::Pair<Integer Pair<Integer Integer>> ->
              if dm.snd.snd == storeDense
              then
                ableC_Stmt {
                  unsigned long $name{s"${nm}${toString(dm.fst+1)}_size"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][0][0];
                  $Stmt{inn}
                }
              else
                ableC_Stmt {
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_pos"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][0];
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_idx"} = $name{nm}.indices[$intLiteralExpr{dm.snd.fst}][1];
                  $Stmt{inn}
                }
            ,
            nullStmt(),
            getTensorFormat(t, fmts).storage
          )
        in
        ableC_Stmt {
          double* $name{s"${nm}_data"} = $name{nm}.data;
          $Stmt{indexSetup}
          $Stmt{inn}
        }
        end end
      ,
      _,
      exNew.tensors
    );

  local dimsCheck :: (Stmt ::= Stmt) =
    let varChecks :: Stmt =
      foldl(
        \ inn::Stmt var::String ->
          ableC_Stmt {
            $Stmt{checkVar(out, exNew, var, fmts)}
            $Stmt{inn}
          }
        ,
        nullStmt(),
        access
      )
    in
    \ inner::Stmt ->
    ableC_Stmt {
      char error = 0;
      $Stmt{varChecks}
      if(error) exit(1);
      $Stmt{inner}
    }
    end;

  local outVal :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
      ableC_Stmt {
        double $name{s"t${head(access)}0"} = 0.0;
        $Stmt{inner}
      };

  local comp :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
    ableC_Stmt {
    {
      $Stmt{computeStmt}
      $Stmt{inner}
    }
    };

  local setFormats :: Stmt =
    foldl(
      \ inn::Stmt nm::String ->
        ableC_Stmt {
          $name{nm}.form = $stringLiteralExpr{exprToString(exNew, fmts)};
          pthread_rwlock_unlock(&($name{nm}.lock));
          $Stmt{inn}
        }
      ,
      nullStmt(),
      map(\p::Pair<String String> -> p.fst, originalNames)
    );

  forwards to
    mkErrorCheck(
      lErrors,
      eqExpr(
        output,
        stmtExpr(
          declExpr(declTensor(packTensors(tensorSub(tensorPrep(dimsCheck(
            outVal(comp(setFormats)))))))),
          ableC_Expr {
            $name{s"t${head(access)}0"}
          },
          location=top.location
        ),
        location=top.location
      )
    );
}

function mergeOrder
Maybe<[String]> ::= orders::[[String]]
{
  local lowers::[String] =
    flatMap(
      \ var::[String] ->
        if null(var)
        then []
        else tail(var)
      ,
      orders
    );

  local top::[String] =
    map(
      \ var::[String] ->
        head(var)
      ,
      orders
    );

  local safe::[Boolean] =
    map(
      \ v::String ->
        !containsBy(stringEq, v, lowers)
      ,
      top
    );

  local vars::[String] =
    filterWith(top, safe);

  local newOrder::[[String]] =
    filter(
      \ lst::[String] -> !null(lst),
      map(
        \ var::[String] ->
          if head(var) == head(vars)
          then tail(var)
          else var
        ,
        orders
      )
    );

  local next::Maybe<[String]> =
    mergeOrder(newOrder);

  return
    if null(vars)
    then nothing()
    else
      if null(newOrder)
      then just(head(vars) :: [])
      else 
        case next of
        | nothing() -> nothing()
        | just(lst) -> just(head(vars) :: lst)
        end;
}

function check_dims
String ::= out::TensorExpr ex::TensorExpr acc::[String] fmts::tm:Map<String TensorFormat>
{
  return
    "char error = 0;"
    ++
    "\n"
    ++
    implode("\n",
      map(
        check_var(out, ex, _, fmts),
        acc
      )
    )
    ++
    "if(error) exit(1);";
}

function checkVar
Stmt ::= out::TensorExpr ex::TensorExpr var::String fmts::tm:Map<String TensorFormat>
{
  out.variable = var;
  ex.variable = var;
  out.fmts = fmts;
  ex.fmts = fmts;

  local acc::[Pair<String Integer>] =
    out.sparse_r ++ out.dense_r ++ ex.sparse_r ++ ex.dense_r;

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
      let checks::Stmt =
        foldl(
          \ inn::Stmt p::Pair<String Integer> ->
            ableC_Stmt {
              if($name{nm}.dims[$intLiteralExpr{h.snd}] != $name{p.fst}.dims[$intLiteralExpr{p.snd}]) {
                fprintf(stderr, $stringLiteralExpr{s"Tensor ${nm} and ${p.fst} do not have the same dimensionality for ${var}.\n"});
                error = 1;
              }
              $Stmt{inn}
            }
          ,
          nullStmt(),
          tail(acc)
        )
      in
      ableC_Stmt {
        unsigned long $name{s"${var}_dimensions"} = $name{nm}.dims[$intLiteralExpr{h.snd}];
        $Stmt{checks}
      }
      end end end;
}

function check_var
String ::= out::TensorExpr ex::TensorExpr var::String fmts::tm:Map<String TensorFormat>
{
  out.variable = var;
  ex.variable = var;
  out.fmts = fmts;
  ex.fmts = fmts;

  local acc::[Pair<String Integer>] =
    out.sparse_r ++ out.dense_r ++ ex.sparse_r ++ ex.dense_r;

  return
    if null(acc)
    then ""
    else
      let h::Pair<String Integer> =
        head(acc)
      in
      let nm::String =
        h.fst
      in
      let dim::String =
        toString(h.snd)
      in
      s"unsigned long ${var}_dimensions = ${nm}.dims[${dim}];"
      ++
      "\n"
      ++
      implode("\n",
        map(
          \ p::Pair<String Integer> ->
            s"if(${nm}.dims[${dim}] != ${p.fst}.dims[${toString(p.snd)}]) {"
            ++
            "\n"
            ++
            s"  fprintf(stderr, \"Tensor ${nm} and ${p.fst} do not have the same dimensionality for ${var}.\\n\");"
            ++
            "\n"
            ++
            "  error = 1;"
            ++
            "\n"
            ++
            "}"
          ,
          tail(acc)
        )
      )
      end
      end
      end;
}

function checkFormats
String ::= fmt::String tensors::[TensorExpr]
{
  return
    case tensors of
    | [] -> "1"
    | x::[] -> s"strcmp(${x.tensorName}.form, \"${fmt}\") != 0"
    | x::tl -> s"strcmp(${x.tensorName}.form, \"${fmt}\") != 0 || ${checkFormats(fmt, tl)}"
    end;
}

function setFormats
String ::= fmt::String tensors::[TensorExpr]
{
  return
    if null(tensors)
    then ""
    else
      let x::TensorExpr =
        head(tensors)
      in
      s"${x.tensorName}.form = \"${fmt}\";"
      ++
      "\n"
      ++
      setFormats(fmt, tail(tensors))
      end;
}

function freeIndices_String
String ::= fmt::TensorFormat
{
  return freeIndices_String_helper(fmt.storage);
}

function freeIndices_String_helper
String ::= strg::[Pair<Integer Pair<Integer Integer>>]
{
  local p :: Pair<Integer Pair<Integer Integer>> =
    head(strg);

  return
    if null(strg)
    then "free(t->indices);"
    else if p.snd.snd == storeDense
    then
      s"""
        free(t->indices[${toString(p.snd.fst)}]);
        ${freeIndices_String_helper(tail(strg))}
      """
    else
      s"""
        free(t->indices[${toString(p.snd.fst)}][0]);
        free(t->indices[${toString(p.snd.fst)}][1]);
        free(t->indices[${toString(p.snd.fst)}]);
        ${freeIndices_String_helper(tail(strg))}
      """;
}
