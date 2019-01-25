grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Production to perform the code generation process and setup
   for a tensor expression where both the left and right hand
   sides are tensor expresssions (e.g. A[i] = B[i,j] * C[j]) 
   where i and j are indexvar's -}
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
    tensorAccess(tensor, idx, top.env, location=top.location);
  
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

  -- New names to remove any duplicate tensor uses
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

  {- Replace names of tensors, as needed -}
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

  {- Any indexvars that appear only on the lhs -}
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

  -- Attempt to determine an order for the indexvars
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

  {- Determine if the expression can be turned into a tensor
     transpose. This is only done if a valid order for the 
     expression cannot be determined, and both lhs and rhs
     are each just a tensorAccess -}
  local isTranspose :: Boolean =
    case order of
    | nothing() ->
      case ex, out of
      | tensorAccess(_, _, _), tensorAccess(_, _, _) ->
        true
      | _, _ -> false
      end
    | _ -> false
    end;

  local lErrors::[Message] =
    if invalidLeftVar
    then [err(top.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(", ", leftOnly)} only occur on the left-hand side.")]
    else []
    ++
    if !isTranspose
    then
      case order of
      | nothing() -> [err(top.location, "Cannot generate code for this tensor expression due to cyclical access patterns")]
      | just(_) -> []
      end
    else [];

  local graph::ComputeGraph =
    computeGraph(
      outNew, fmts, exNew, access,
      top.location, top.env
    );

  {- Lookup to see whether the programmer enabled OpenMP
     parallelization -}
  local canParallel :: Boolean =
    case lookupValue(emitParallel, top.env) of
    | [] -> false
    | _::_ -> true
    end;

  {- Lookup to see whether the programmer specified a 
     number of threads to use for OpenMP parallelization,
     if so, read out the value if possible (Since OpenMP
     is a compiler addon, we must specify the number of
     threads as an integer constant) -}
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

  -- Procedural name of the output tensor's format
  local fmtNm::String =
    getTensorFormat(outNew, fmts).proceduralName;

  -- The order of the output tensor
  local outOrder::Integer = 
    getTensorFormat(outNew, fmts).dimensions;

  exNew.fmts = fmts;
  outNew.fmts = fmts;

  {- Lock the output tensor with a write lock. This is a 
     lambda allowing us to pass the body of the code gen
     to it, and that will safely be wrapped by the locking -}
  local lockOut :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
      ableC_Stmt {
        $Stmt{
          case outNew of
          | tensorAccess(ex, _, _) ->
            case ex of
            | decExpr(declRefExpr(name(_))) -> nullStmt()
            | declRefExpr(name(_)) -> nullStmt()
            | _ -> 
              ableC_Stmt {
                struct $name{s"tensor_${fmtNm}"} $name{getTensorName(outNew)} = 
                    (struct $name{s"tensor_${fmtNm}"}) $Expr{ex};
              }
            end
          end
        }
        pthread_rwlock_wrlock(&(((struct $name{s"tensor_${getTensorFormat(outNew, fmts).proceduralName}"}*) 
          &$name{outNew.tensorName})->lock));
        $Stmt{inner}
        pthread_rwlock_unlock(&(((struct $name{s"tensor_${getTensorFormat(outNew, fmts).proceduralName}"}*) 
          &$name{outNew.tensorName})->lock));
      };

  {- A list of all Expr's used in the tensor expression that must 
     be given a temporary name to prevent wasting computation time -}
  local exprs :: [Pair<String Expr>] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | decExpr(declRefExpr(name(_))) -> nothing()
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          just(pair(getExprName(e, top.env), e))
        end
      ,
      exNew.exprs
    );

  {- Declare any Expr's that need to be given an internal name -}
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

  {- If any tensor is not just a name, we must store it in a variable -}
  local preSubs :: [Pair<Pair<String String> Expr>] = -- (newName, fmt), expr
    maybeMap(
      \ t::TensorExpr ->
        case t of
        | tensorAccess(ex, _, _) ->
          case decorate ex with{env=top.env; returnType=nothing();} of
          | decExpr(declRefExpr(name(_))) -> nothing()
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
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = (struct $name{s"tensor_${p.fst.snd}"}) $Expr{p.snd};
          $Stmt{inn}
        }
      ,
      _,
      preSubs
    );

  {- All the tensors used, without repeats -}
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

  {- Pack all of the input tensors, and aquire read-locks -}
  local packTensors :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::Pair<String String> ->
        ableC_Stmt {
          $name{s"tensor_pack_${t.snd}"}((struct $name{s"tensor_${t.snd}"}*) &$name{t.fst});
          pthread_rwlock_rdlock(&( ((struct $name{s"tensor_${t.snd}"}*) &$name{t.fst})->lock));
          $Stmt{inn}
        }
      ,
      _,
      originalNames
    );

  {- Find any tensor's that are used multiple times. We rename
     these so that the codegen system does not have to handled
     these tensors specially -}
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
    \ inner::Stmt ->
    foldl(
      \ inn::Stmt p::Pair<Pair<String String> String> ->
        ableC_Stmt {
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = (struct $name{s"tensor_${p.fst.snd}"}) $name{p.snd};
          $Stmt{inn}
        }
      ,
      inner,
      postSubs
    );

  {- Declare sizes and position and index arrays for each tensor and
     dimension -}
  local tensorPrep :: (Stmt ::= Stmt) =
    foldl(
      \ inn::Stmt t::TensorExpr ->
        let nm::String = getTensorName(t) in
        let fmtNm::String = getTensorFormat(t, fmts).proceduralName in
        let indexSetup :: Stmt =
          foldl(
            \ inn::Stmt dm::Pair<Integer Pair<Integer Integer>> ->
              if dm.snd.snd == storeDense
              then
                ableC_Stmt {
                  unsigned long $name{s"${nm}${toString(dm.fst+1)}_size"} = 
                    ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{dm.snd.fst}][0][0];
                  $Stmt{inn}
                }
              else
                ableC_Stmt {
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_pos"} = 
                    ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{dm.snd.fst}][0];
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_idx"} = 
                    ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{dm.snd.fst}][1];
                  $Stmt{inn}
                }
            ,
            nullStmt(),
            getTensorFormat(t, fmts).storage
          )
        in
        ableC_Stmt {
          double* $name{s"${nm}_data"} = ((struct $name{s"tensor_${fmtNm}"})$name{nm}).data;
          $Stmt{indexSetup}
          $Stmt{inn}
        }
        end end end
      ,
      _,
      exNew.tensors
    );

  {- Check that all dimensions accessed by the same indexvar
     have the same length -}
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

  {- Assemble the output tensor, this uses the computeGraph's asmbl
     attribute, as well as some other setup code provided here. It 
     also makes use of some of the code generation used for declaring
     the pack function. -}
  local assembleOut :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
      if allDense(getTensorFormat(outNew, fmts))
      then
        ableC_Stmt {
          memset(
            ((struct $name{s"tensor_${fmtNm}"})$name{outNew.tensorName}).data, 0, 
            ((struct $name{s"tensor_${fmtNm}"})$name{outNew.tensorName}).dataLen * sizeof(double));
          $Stmt{inner}
        }
      else
        ableC_Stmt {
          unsigned long idx[$intLiteralExpr{listLength(head(out.accesses))}];
          struct $name{s"tensor_${fmtNm}"}* t = (struct $name{s"tensor_${fmtNm}"}*) &$name{outNew.tensorName};
          unsigned long count = 1;
          
          if(t->indices) { $Stmt{freeIndicesTPointer(getTensorFormat(outNew, fmts))} }
          t->indices = calloc($intLiteralExpr{outOrder}, sizeof(unsigned long**));
          $Stmt{generateMakeBody(getTensorFormat(outNew, fmts).storage)}

          __free_tensor_tree(t->buffer);
          t->buffer = calloc(1, sizeof(struct __tensor_tree));
          t->buffer->children = calloc(1, sizeof(struct __tensor_tree));

          $Stmt{graph.asmbl}

          unsigned long* dims = ((struct $name{s"tensor_${fmtNm}"})$name{outNew.tensorName}).dims;
          $name{s"tensor_packTree_${fmtNm}"}(
            ((struct $name{s"tensor_${fmtNm}"})$name{outNew.tensorName}).buffer, dims);

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

  {- Declare the sizes and index and position arrays for the output tensor -}
  local outVal :: (Stmt ::= Stmt) =
    let nm :: String =
      getTensorName(head(outNew.tensors))
    in
    let fmtNm :: String = 
      getTensorFormat(head(outNew.tensors), fmts).proceduralName 
    in
    let setupOut :: Stmt =
      foldl(
        \ inn::Stmt p::Pair<Integer Pair<Integer Integer>> ->
          if p.snd.snd == storeDense
          then
            ableC_Stmt {
              unsigned long $name{s"${nm}${toString(p.fst+1)}_size"} = 
                ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{p.snd.fst}][0][0];
              $Stmt{inn}
            }
          else
            ableC_Stmt {
              unsigned long* $name{s"${nm}${toString(p.fst+1)}_pos"} = 
                ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{p.snd.fst}][0];
              unsigned long* $name{s"${nm}${toString(p.fst+1)}_idx"} = 
                ((struct $name{s"tensor_${fmtNm}"})$name{nm}).indices[$intLiteralExpr{p.snd.fst}][1];
              $Stmt{inn}
            }
        ,
        nullStmt(),
        getTensorFormat(head(outNew.tensors), fmts).storage
      )
    in
    \ inner::Stmt ->
    ableC_Stmt {
      double* $name{s"${nm}_data"} = ((struct $name{s"tensor_${fmtNm}"})$name{nm}).data;
      $Stmt{setupOut}
      $Stmt{inner}
    }
    end end end;

  {- Actually use the generate compute code -}
  local comp :: (Stmt ::= Stmt) =
    \ inner::Stmt ->
    ableC_Stmt {
    { // Extra set of braces needed to avoid redeclaration from assemble
      $Stmt{computeStmt}
      $Stmt{inner}
    }
    };

  {- We set the .form field of each tensor to a string representing this
     expression, hoping to use this to avoid unecessary packing and assembly
     when it can be avoided. This also releases the locks on the input 
     tensors -}
  local setFormat :: Stmt =
    foldl(
      \ inn::Stmt pr::Pair<String String> ->
        ableC_Stmt {
          ((struct $name{s"tensor_${pr.snd}"}*) &$name{pr.fst})->form = $stringLiteralExpr{exprToString(exNew, fmts)};
          pthread_rwlock_unlock(&(((struct $name{s"tensor_${pr.snd}"}*) &$name{pr.fst})->lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        ((struct $name{s"tensor_${getTensorFormat(head(outNew.tensors), fmts).proceduralName}"}*)
          &$name{getTensorName(head(outNew.tensors))})->form = $stringLiteralExpr{exprToString(exNew, fmts)};
      },
      originalNames
    );

  local fwrd :: Expr =
    if isTranspose
    then -- If this is a tranpose, we perform the basic setup, and then transpose
      stmtExpr(
        lockOut(declTensor(tensorSub(
          ableC_Stmt {
            $Expr{transpose(outNew.tensorName, head(outNew.accesses), getTensorFormat(outNew, fmts), exNew.tensorName, head(exNew.accesses), getTensorFormat(exNew, fmts), location=top.location)};
            $Stmt{setFormat}
          }))),
        ableC_Expr { $name{outNew.tensorName} },
        location=top.location
      )
    else
      stmtExpr(
        lockOut(declTensor(packTensors(declExpr(tensorSub(tensorPrep(dimsCheck(
          assembleOut(outVal(comp(setFormat)))))))))),
        ableC_Expr {
          $name{outNew.tensorName}
        },
        location=top.location
      );

  forwards to
    mkErrorCheck(
      lErrors,
      fwrd
    );
}

{- Code generation system for when the output of the tensor
   expression is just a scalar (e.g. s = x[i] * y[j];) -}
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
        | tensorAccess(ex, _, _) ->
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
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = (struct $name{s"tensor_${p.fst.snd}"}) $Expr{p.snd};
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
          $name{s"tensor_pack_${t.snd}"}((struct $name{s"tensor_${t.snd}"}*) &$name{t.fst});
          pthread_rwlock_rdlock(&(((struct $name{s"tensor_${t.snd}"}*) &$name{t.fst})->lock));
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
          struct $name{s"tensor_${p.fst.snd}"} $name{p.fst.fst} = (struct $name{s"tensor_${p.fst.snd}"}) $name{p.snd};
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
        let fmtNm::String = 
          getTensorFormat(t, fmts).proceduralName
        in
        let indexSetup :: Stmt =
          foldl(
            \ inn::Stmt dm::Pair<Integer Pair<Integer Integer>> ->
              if dm.snd.snd == storeDense
              then
                ableC_Stmt {
                  unsigned long $name{s"${nm}${toString(dm.fst+1)}_size"} = 
                    ((struct $name{s"tensor_${fmtNm}"}) $name{nm}).indices[$intLiteralExpr{dm.snd.fst}][0][0];
                  $Stmt{inn}
                }
              else
                ableC_Stmt {
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_pos"} = 
                    ((struct $name{s"tensor_${fmtNm}"}) $name{nm}).indices[$intLiteralExpr{dm.snd.fst}][0];
                  unsigned long* $name{s"${nm}${toString(dm.fst+1)}_idx"} = 
                    ((struct $name{s"tensor_${fmtNm}"}) $name{nm}).indices[$intLiteralExpr{dm.snd.fst}][1];
                  $Stmt{inn}
                }
            ,
            nullStmt(),
            getTensorFormat(t, fmts).storage
          )
        in
        ableC_Stmt {
          double* $name{s"${nm}_data"} = ((struct $name{s"tensor_${fmtNm}"}) $name{nm}).data;
          $Stmt{indexSetup}
          $Stmt{inn}
        }
        end end end
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

  {- If the output tensor is a scalar, the code gen system
     will store the final value in tx0 (where x is the first
     indexvar in the order). This is due to the recursive
     nature of the algorithm, and is the easiest way -}
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
      \ inn::Stmt pr::Pair<String String> ->
        ableC_Stmt {
          ((struct $name{s"tensor_${pr.snd}"}*) &$name{pr.fst})->form = $stringLiteralExpr{exprToString(exNew, fmts)};
          pthread_rwlock_unlock(&(((struct $name{s"tensor_${pr.snd}"}*) &$name{pr.fst})->lock));
          $Stmt{inn}
        }
      ,
      nullStmt(),
      originalNames
    );

  forwards to
    mkErrorCheck(
      lErrors,
      eqExpr(
        output, -- set output equal to ...
        stmtExpr( -- perform the compute
          declExpr(declTensor(packTensors(tensorSub(tensorPrep(dimsCheck(
            outVal(comp(setFormats)))))))),
          ableC_Expr { -- the value tx0
            $name{s"t${head(access)}0"}
          },
          location=top.location
        ),
        location=top.location
      )
    );
}

{- Attempts to merge the access orders from several tensors
   into a single valid order -}
function mergeOrder
Maybe<[String]> ::= orders::[[String]]
{
  -- A list of all indexvar's that appear in a list after
  -- the first element (meaning they cannot be the next
  -- element in the order).
  local lowers::[String] =
    flatMap(
      \ var::[String] ->
        if null(var)
        then []
        else tail(var)
      ,
      orders
    );

  -- A list of all variables appearing as the first element
  -- of any of the accesses
  local top::[String] =
    map(
      \ var::[String] ->
        head(var)
      ,
      orders
    );

  -- Whether or not a variable at the beginning of a list 
  -- can be next (meaning it is not in the lowers list)
  local safe::[Boolean] =
    map(
      \ v::String ->
        !containsBy(stringEq, v, lowers)
      ,
      top
    );

  -- Taking the top list and safe list, produce a list of 
  -- the variables that can be the next element of the list
  local vars::[String] =
    filterWith(top, safe);

  -- Remove the first element of vars from all lists, and 
  -- remove any now empty lists
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

  -- Perform a recursive merge call
  local next::Maybe<[String]> =
    mergeOrder(newOrder);

  return
    if null(vars) -- if there is no variable that can be next
    then nothing()
    else
      if null(newOrder) -- if we're done now
      then just(head(vars) :: [])
      else 
        case next of -- ensure recursive call is successful
        | nothing() -> nothing()
        | just(lst) -> just(head(vars) :: lst)
        end;
}

{- Function to check that dimensions that need to match have the same
   size -}
function checkVar
Stmt ::= out::TensorExpr ex::TensorExpr var::String 
         fmts::tm:Map<String TensorFormat> loc::Location
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
      let fmtNm0 :: String = head(tm:lookup(nm, fmts)).proceduralName in
      let checks::Stmt =
        foldl(
          \ inn::Stmt p::Pair<String Integer> ->
            let fmtNm1 :: String =
               head(tm:lookup(p.fst, fmts)).proceduralName
            in
            ableC_Stmt {
              if(((struct $name{s"tensor_${fmtNm0}"}) $name{nm}).dims[$intLiteralExpr{h.snd}] 
                != ((struct $name{s"tensor_${fmtNm1}"}) $name{p.fst}).dims[$intLiteralExpr{p.snd}]) {
                fprintf(stderr, 
                  $stringLiteralExpr{s"Tensor ${nm} and ${p.fst} do not have the same dimensionality for ${var}. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n"});
                error = 1;
              }
              $Stmt{inn}
            }
            end
          ,
          nullStmt(),
          tail(acc)
        )
      in
      ableC_Stmt {
        unsigned long $name{s"${var}_dimensions"} = ((struct $name{s"tensor_${fmtNm0}"}) $name{nm}).dims[$intLiteralExpr{h.snd}];
        $Stmt{checks}
      }
      end end end end;
}
