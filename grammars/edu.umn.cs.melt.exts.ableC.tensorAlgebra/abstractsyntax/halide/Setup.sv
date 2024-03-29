grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Production that provides the necessary tensor setup before a 
   halide tensor expression. This provides error checking, locking,
   etc. -}
abstract production halideSetup
top::Stmt ::= tensor::Expr idx::Expr value::Expr inner::Stmt
{
  top.pp = text("// Halide Tensor Expr Setup");
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext;
  value.env = top.env;

  local out::TensorExpr = -- Build the output into a TensorExpr
    tensorAccess(tensor, idx, top.env);
  local ex::TensorExpr = -- Get the rhs's TensorExpr
    value.tensorExp;

  local tensors::[TensorExpr] = -- All the tensors used in this equation
    ex.tensors ++ out.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local tensorFormats::[TensorFormat] =
    map(
      \ e::TensorExpr ->
        getTensorFormat(e, tm:empty())
      ,
      tensors
    );

  -- Generate new names to resolve name conflicts, thus
  -- allowing equations like a[i] = b[i] * b[i];
  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer = 
          count(n, o)
        in
        if c > 0
        then n ++ toString(c) ++ "_"
        else n
        end
      ,
      tensorNames
    );

  -- Replace any duplicated names that need to be
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

  local access :: [String] = nub(concat(out.accesses ++ ex.accesses));

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zip(
        newNames,
        tensorFormats
      ),
      tm:empty()
    );

  local originalNames :: [String] = nub(map(getTensorName, ex.tensors));

  {- If any tensor is an Expr other than a declRefExpr,
     declare it with a generated name, so as to only
     use the Expr once -}
  local tensorDecls :: [Stmt] =
    maybeMap(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(ex, _, _) ->
          case decorate ex with {env=e.envr;
                            controlStmtContext=initialControlStmtContext;} of
          | decExpr(declRefExpr(name(_))) -> nothing()
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
                struct $name{s"tensor_${fmt.proceduralName}"} $name{nm} = (struct $name{s"tensor_${fmt.proceduralName}"}) $Expr{ex};
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

  {- If there are any Expr in the equation that aren't simply
     declRefExpr, e.g. '2', give them names -}
  local exprDecls :: [Stmt] =
    maybeMap(
      \ e::Expr ->
        case decorate e with {env=top.env;
                            controlStmtContext=initialControlStmtContext;} of
        | decExpr(declRefExpr(name(_))) -> nothing()
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

  {- Lock all tensors. Read locks for input tensors,
     write locks for the output tensor -}
  local requestLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_rdlock(
            &(((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"}*) &$name{t})->lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        pthread_rwlock_wrlock(&(((struct $name{s"tensor_${head(tm:lookup(outNew.tensorName, fmts)).proceduralName}"}*)
          &$name{outNew.tensorName})->lock));
      },
      originalNames
    );

  local releaseLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_unlock(&(((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"}*) &$name{t})
            ->lock));
          $Stmt{inn}
        }
      ,
      ableC_Stmt {
        pthread_rwlock_unlock(&(((struct $name{s"tensor_${head(tm:lookup(outNew.tensorName, fmts)).proceduralName}"}*)
          &$name{outNew.tensorName})->lock));
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

  {- Declare new names for any tensors that are used twice, so as to 
     prevent name conflicts -}
  local tensorNameSub :: Stmt =
    foldl(
      \ s1::Stmt pr::Pair<String Pair<String String>> ->
        seqStmt(s1,
          if pr.snd.fst == pr.snd.snd
          then
            nullStmt()
          else
            ableC_Stmt {
              struct $name{s"tensor_${pr.fst}"} $name{pr.snd.fst} = (struct $name{s"tensor_${pr.fst}"}) $name{pr.snd.snd};
            }
        )
      ,
      nullStmt(),
      zip(
        map(
          \ f::TensorFormat ->
            f.proceduralName
          ,
          tensorFormats
        ),
        zip(newNames, tensorNames)
      )
    );

  {- Check that all dimensions accessed using the same indexvar have
     the same length -}
  local checkDims :: Stmt =
    halide_check_dims(out, exNew, access, fmts);

  local initData :: Stmt =
    foldl(
      \ s1::Stmt t::String ->
        seqStmt(
         s1,
         ableC_Stmt {
           double* $name{s"${t}_data"} = ((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"}) $name{t}).data;
         }
        )
      ,
      nullStmt(),
      newNames
    );

  local zeroOut :: Stmt =
    ableC_Stmt {
      memset(((struct $name{s"tensor_${head(tm:lookup(outNew.tensorName, fmts)).proceduralName}"}*) 
        &$name{outNew.tensorName})->data, 0, 
          ((struct $name{s"tensor_${head(tm:lookup(outNew.tensorName, fmts)).proceduralName}"}*) 
            &$name{outNew.tensorName})->dataLen * sizeof(double));
    };

  local lErrors :: [Message] =
    checkTensorHeader(top.env);

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
  fwrd.controlStmtContext = top.controlStmtContext;

  inner.env = 
    addEnv(
      map(
        \ v::String ->
          valueDef(
            v ++ "_dimension",
            builtinValueItem(
              builtinType(
                nilQualifier(),
                unsignedType(longType())
              )
            )
          )
        ,
        access
      )
      ++
      map(
        \ nm::String ->
          valueDef(
            nm ++ "_data",
            builtinValueItem(
              pointerType(
                nilQualifier(),
                builtinType(
                  nilQualifier(),
                  realType(doubleType())
                )
              )
            )
          )
        ,
        newNames
      ),
      top.env
    );

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else if !null(inner.errors)
    then warnStmt(inner.errors)
    else fwrd;
}

{- Setup for a Halide expression with the output being a scalar. 
   Very similar to the setup above, though an additional output 
   variable is declared, and anything involving the lhs has been
   removed from the other setup operations. -}
abstract production halideScalarSetup
top::Stmt ::= output::Name expr::Expr inner::Stmt
{
  top.pp = text("// Halide Tensor Expr Setup");
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext;
  expr.env = top.env;

  local ex::TensorExpr =
    expr.tensorExp;

  local tensors::[TensorExpr] =
    ex.tensors;

  local tensorNames::[String] =
    map(
      getTensorName,
      tensors
    );

  local originalNames :: [String] = nub(tensorNames);

  local requestLocks :: Stmt =
    foldl(
      \ inn::Stmt t::String ->
        ableC_Stmt {
          pthread_rwlock_rdlock(&(((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"}*) &$name{t})
            ->lock));
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
          pthread_rwlock_unlock(&(((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"}*) &$name{t})
            ->lock));
          $Stmt{inn}
        }
      ,
      nullStmt(),
      originalNames
    );

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty()),
      tensors
    );

  local newNames::[String] =
    mapWithTail(
      \ n::String o::[String] ->
        let c::Integer =
          count(n, o)
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
          "__out__"
        )
      ),
      top.env
    );

  local access::[String] = nub(concat(ex.accesses));

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zip(
        newNames,
        tensorFormats
      ),
      tm:empty()
    );

  local tensorDecls :: [Stmt] =
    maybeMap(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(ex, _, _) ->
          case decorate ex with {env=e.envr;
                        controlStmtContext = initialControlStmtContext;} of
          | decExpr(declRefExpr(name(_))) -> nothing()
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
                struct $name{s"tensor_${fmt.proceduralName}"} $name{nm} = (struct $name{s"tensor_${fmt.proceduralName}"}) $Expr{ex};
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
        case decorate e with {env=top.env;
                        controlStmtContext = initialControlStmtContext;} of
        | decExpr(declRefExpr(name(_))) -> nothing()
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
              struct $name{s"tensor_${pr.fst}"} $name{pr.snd.fst} = (struct $name{s"tensor_${pr.fst}"}) $name{pr.snd.snd};
            }
        )
      ,
      nullStmt(),
      zip(
        map(
          \ f::TensorFormat ->
            f.proceduralName
          ,
          tensorFormats
        ),
        zip(newNames, tensorNames)
      )
    );

  local initData :: Stmt =
    foldl(
      \ s1::Stmt t::String ->
        ableC_Stmt {
          $Stmt{s1}
          double* $name{s"${t}_data"} = ((struct $name{s"tensor_${head(tm:lookup(t, fmts)).proceduralName}"})$name{t})
            .data;
        }
      ,
      nullStmt(),
      newNames
    );

  local checkDims :: Stmt =
    halide_check_dims(out, exNew, access, fmts);

  local lErrors :: [Message] =
    checkTensorHeader(top.env);

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
        name("__result")
      )
    );

  local finalFwrd :: Stmt =
    exprStmt(
      eqExpr(
        declRefExpr(
          output -- set output (a name) equal to
        ),
        stmtExpr(
          ableC_Stmt {
            double __result = 0.0; // declare __result
          },
          fwrd -- the Expr generated above
        )
      )
    );

  finalFwrd.controlStmtContext = top.controlStmtContext;
  finalFwrd.env = top.env;

  inner.env = 
    addEnv(
      valueDef(
        "__result",
        builtinValueItem(
          builtinType(
            nilQualifier(),
            realType(doubleType())
          )
        )
      )
      ::
      map(
        \ v::String ->
          valueDef(
            v ++ "_dimension",
            builtinValueItem(
              builtinType(
                nilQualifier(),
                unsignedType(longType())
              )
            )
          )
        ,
        access
      )
      ++
      map(
        \ nm::String ->
          valueDef(
            nm ++ "_data",
            builtinValueItem(
              pointerType(
                nilQualifier(),
                builtinType(
                  nilQualifier(),
                  realType(doubleType())
                )
              )
            )
          )
        ,
        newNames
      ),
      top.env
    );
  
  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else if !null(inner.errors)
    then warnStmt(inner.errors)
    else finalFwrd;

}

{- Check that all dimensions of all tensors in a  tensor 
   expression, have the same size if accessed with the same
   indexvar -}
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
          if(((struct $name{s"tensor_${head(tm:lookup(nm, fmts)).proceduralName}"}) $name{nm})
            .dims[$intLiteralExpr{h.snd}] 
              != ((struct $name{s"tensor_${head(tm:lookup(pr.fst, fmts)).proceduralName}"})$name{pr.fst})
                .dims[$intLiteralExpr{pr.snd}]) {
            fprintf(stderr, 
              $stringLiteralExpr{s"Tensor ${nm} and ${pr.fst} do not have the same dimensionality for ${var}. (At ${getParsedOriginLocationOrFallback(out).unparse})\n"});
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
        unsigned long $name{s"${var}_dimension"} = 
          ((struct $name{s"tensor_${head(tm:lookup(nm, fmts)).proceduralName}"}) $name{nm})
            .dims[$intLiteralExpr{h.snd}];
        $Stmt{check}
      }
      end
      end;
}
