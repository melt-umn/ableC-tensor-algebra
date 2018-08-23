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

  local canParallel :: Boolean =
    case lookupValue(emitParallel, top.env) of
    | [] -> false
    | _::_ -> true
    end;

  local thdCnt :: Maybe<Integer> =
    case lookupValue(emitThreads, top.env) of
    | [] -> nothing()
    | v::_ ->
      case v of
      | declaratorValueItem(
          declarator(_, _, _, 
            justInitializer(
              exprInitializer(
                realConstant(
                  integerConstant(n, _, _)))))) -> toIntSafe(n)
      | _ -> nothing()
      end
    end;

  graph.canPar = canParallel;
  graph.thdCnt = thdCnt;

  local computeStmt::Stmt =
    graph.compute;

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
            $Stmt{checkVar(outNew, exNew, var, fmts, top.location)}
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
          __free_tensor_tree(t->buffer);
          t->buffer = calloc(1, sizeof(struct __tensor_tree));
          t->buffer->children = calloc(1, sizeof(struct __tensor_tree));

          $Stmt{graph.asmbl}

          unsigned long* dims = $name{outNew.tensorName}.dims;
          $name{s"tensor_packTree_${fmtNm}"}($name{outNew.tensorName}.buffer, dims);

          struct __tensor_tree* buffer = t->buffer;

          if(t->indices) { $Stmt{freeIndicesTPointer(getTensorFormat(outNew, fmts))} }
          t->indices = calloc($intLiteralExpr{outOrder}, sizeof(unsigned long**));

          unsigned long numChildren = 1;
          struct __tensor_tree** trees = &buffer;

          struct __tensor_tree** temp_tree;
          unsigned long total, dimSize, index, newChildren;

          $Stmt{generatePackBody_Assemble(getTensorFormat(outNew, fmts).storage)}
          
          if(t->data) free(t->data);
          t->data = calloc(numChildren, sizeof(double));
          for(unsigned long i = 0; i < numChildren; i++) {
            t->data[i] = trees[i]->val;
          }
          if(trees != &buffer) free(trees);

          __free_tensor_tree(t->buffer);
          t->dataLen = numChildren;
          t->bufferCnt = 0;
          t->buffer = calloc(1, sizeof(struct __tensor_tree));
          t->buffer->children = calloc(1, sizeof(struct __tensor_tree));
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
  
  local canParallel :: Boolean =
    case lookupValue(emitParallel, top.env) of
    | [] -> false
    | _::_ -> true
    end;

  local thdCnt :: Maybe<Integer> =
    case lookupValue(emitThreads, top.env) of
    | [] -> nothing()
    | v::_ -> 
      case v of
      | declaratorValueItem(
          declarator(_, _, _, 
            justInitializer(
              exprInitializer(
                realConstant(
                  integerConstant(n, _, _)))))) -> toIntSafe(n)
      | _ -> nothing()
      end
    end;
  
  graph.canPar = canParallel;
  graph.thdCnt = thdCnt;

  local computeStmt::Stmt =
    graph.compute;

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
            $Stmt{checkVar(out, exNew, var, fmts, top.location)}
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

function checkVar
Stmt ::= out::TensorExpr ex::TensorExpr var::String fmts::tm:Map<String TensorFormat> loc::Location
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
                fprintf(stderr, 
                  $stringLiteralExpr{s"Tensor ${nm} and ${p.fst} do not have the same dimensionality for ${var}. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n"});
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
