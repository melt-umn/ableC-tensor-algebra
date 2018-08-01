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

  local assembleStmt::Stmt =
    if allDense(getTensorFormat(outNew, fmts))
    then parseStmt(
      s"memset(${outNew.tensorName}.data, 0, ${outNew.tensorName}.dataLen * sizeof(double));"
    )
    else
    parseStmt(
      s"unsigned long* idx = GC_malloc(sizeof(unsigned long) * ${toString(listLength(head(out.accesses)))});\n"
      ++
      s"struct tensor_${fmtNm}* t = &${out.tensorName};\n"
      ++
      s"t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(outOrder)});\n"
      ++
      "unsigned long count = 1;\n"
      ++
      generateMakeBody(getTensorFormat(outNew, fmts).storage)
      ++
      graph.asmbl
      ++
      s"""
      unsigned long* dims = ${outNew.tensorName}.dims;
      tensor_packTree_${fmtNm}(&(${outNew.tensorName}.buffer), dims);
      
      struct tensor_tree_s* buffer = &(${outNew.tensorName}.buffer);
      t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(outOrder)});
      unsigned long numChildren = 1;
      struct tensor_tree_s** trees = &buffer;

      struct tensor_tree_s** temp_tree;
      unsigned long total, dimSize, index, newChildren;

      ${generatePackBody_Assemble(getTensorFormat(outNew, fmts).storage)}

      t->data = GC_malloc(sizeof(double) * numChildren);
      for(unsigned long i = 0; i < numChildren; i++) {
        t->data[i] = trees[i]->val;
      }

      t->dataLen = numChildren;
      t->bufferCnt = 0;
      t->buffer.numChildren = 0;
      t->buffer.children = 0;
      t->form = "";
      """
    );

  local tensorInit :: [Maybe<Pair<String Expr>>] =
    map(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(_, ex, _, _) ->
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
              pair(
                s"struct tensor_${fmt.proceduralName} ${nm} = __tensor_sub;",
                ex
              )
            )
            end
            end
          end
        | _ -> nothing()
        end
      ,
      exNew.tensors ++ outNew.tensors
    );

  local exprInit :: [Maybe<Pair<String Expr>>] =
    map(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          let nm::String =
            getExprName(e, top.env)
          in
          just(
            pair(
              s"double ${nm} = __expr_sub;",
              e
            )
          )
          end
        end
      ,
      exNew.exprs
    );

  local tensorDecls :: [Stmt] =
    map(
      \ m::Maybe<Pair<String Expr>> ->
        case m of
        | just(pair(s, ex)) ->
          substStmt(
            [declRefSubstitution("__tensor_sub", ex)]
            ,
            parseStmt(s)
          )
        | nothing() ->
          nullStmt()
        end
      ,
      tensorInit
    );

  local exprDecls :: [Stmt] =
    map(
      \ m::Maybe<Pair<String Expr>> ->
        case m of
        | just(pair(s, ex)) ->
          substStmt(
            [declRefSubstitution("__expr_sub", ex)]
            ,
            parseStmt(s)
          )
        | nothing() ->
          nullStmt()
        end
      ,
      exprInit
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

  local tensorValInit :: [Stmt] =
    map(
      \ e::TensorExpr ->
        parseStmt(
          generateTensorVals(e, fmts)
        )
      ,
      exNew.tensors
    );

  local tensorValDec :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      tensorValInit
    );

  local outValInit :: [Stmt] =
    map(
      \ e::TensorExpr ->
        parseStmt(
          generateTensorVals(e, fmts)
        )
      ,
      outNew.tensors
    );

  local outValDec :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      outValInit
    );

  local checkDims :: Stmt =
    parseStmt(
      check_dims(outNew, exNew, access, fmts)
    );

  exNew.fmts = fmts;
  outNew.fmts = fmts;

  local tensorPack :: Stmt =
    foldl(
      \ s1::Stmt e::TensorExpr ->
        seqStmt(s1,
          let fmt::String =
            getTensorFormat(e, fmts).proceduralName
          in
          parseStmt(
            s"tensor_pack_${fmt}(&${e.tensorName});"
          )
          end
        )
      ,
      nullStmt(),
      exNew.tensors
    );

  local tensorNameSub :: Stmt =
    foldl(
      \ s1::Stmt pr::Pair<String Pair<String String>> ->
        seqStmt(s1,
          if pr.snd.fst == pr.snd.snd
          then
            nullStmt()
          else
            parseStmt(
              s"struct tensor_${pr.fst} ${pr.snd.fst} = ${pr.snd.snd};"
            )
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

  forwards to
    mkErrorCheck(
      lErrors,
      substExpr(
        stmtSubstitution("__tensor_decl", tensorDecl) ::
        stmtSubstitution("__tensor_sub", tensorNameSub) ::
        stmtSubstitution("__expr_decl", exprDecl) ::
        stmtSubstitution("__tensor_prep", tensorValDec) ::
        stmtSubstitution("__check_dims", checkDims) ::
        stmtSubstitution("__tensor_pack", tensorPack) ::
        stmtSubstitution("__assemble", assembleStmt) ::
        stmtSubstitution("__out_prep", outValDec) ::
        stmtSubstitution("__compute", computeStmt) ::
        []
        ,
        parseExpr(s"""
        ({
          __tensor_decl;
          __tensor_sub;
          __expr_decl;
          __tensor_pack;
          __tensor_prep;
          __check_dims;
          if(${checkFormats(exprToString(exNew, fmts), exNew.tensors ++ outNew.tensors)}) {
            {__assemble;}
          } else {
            memset(${outNew.tensorName}.data, 0, ${outNew.tensorName}.dataLen * sizeof(double));
          }
          __out_prep;
          __compute;
          ${setFormats(exprToString(exNew, fmts), ex.tensors ++ out.tensors)}
          ${outNew.tensorName};
        })
        """)
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

  local tensorInit :: [Maybe<Pair<String Expr>>] =
    map(
      \ e::TensorExpr ->
        case e of
        | tensorAccess(_, ex, _, _) ->
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
              pair(
                s"struct tensor_${fmt.proceduralName} ${nm} = __tensor_sub;",
                ex
              )
            )
            end
            end
          end
        | _ -> nothing() 
        end
      ,
      exNew.tensors
    );

  local exprInit :: [Maybe<Pair<String Expr>>] =
    map(
      \ e::Expr ->
        case decorate e with {env=top.env; returnType=nothing();} of
        | declRefExpr(name(_)) -> nothing()
        | _ ->
          let nm::String =
            getExprName(e, top.env)
          in
          just(
            pair(
              s"double ${nm} = __expr_sub;",
              e
            )
          )
          end
        end
      ,
      exNew.exprs
    );

  local tensorDecls :: [Stmt] =
    map(
      \ m::Maybe<Pair<String Expr>> ->
        case m of
        | just(pair(s, ex)) ->
          substStmt(
            [declRefSubstitution("__tensor_sub", ex)]
            ,
            parseStmt(s)
          )
        | nothing() -> nullStmt()
        end
      ,
      tensorInit
    );

  local exprDecls :: [Stmt] =
    map(
      \ m::Maybe<Pair<String Expr>> ->
        case m of
        | just(pair(s, ex)) ->
          substStmt(
            [declRefSubstitution("__expr_sub", ex)]
            ,
            parseStmt(s)
          )
        | nothing() -> nullStmt()
        end
      ,
      exprInit
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

  local tensorValInit :: [Stmt] =
    map(
      \ e::TensorExpr ->
        parseStmt(
          generateTensorVals(e, fmts)
        )
      ,
      exNew.tensors
    );

  local tensorValDec :: Stmt =
    foldl(
      \ s1::Stmt s2::Stmt ->
        seqStmt(s1, s2)
      ,
      nullStmt(),
      tensorValInit
    );

  local checkDims :: Stmt =
    parseStmt(
      check_dims(out, exNew, access, fmts)
    );

  exNew.fmts = fmts;

  local tensorPack :: Stmt =
    foldl(
      \ s1::Stmt e::TensorExpr ->
        seqStmt(s1,
          let fmt::String = 
            getTensorFormat(e, fmts).proceduralName
          in
          parseStmt(
            s"tensor_pack_${fmt}(&${e.tensorName});"
          )
          end
        )
      ,
      nullStmt(),
      exNew.tensors
    );

  local tensorNameSub :: Stmt =
    foldl(
      \ s1::Stmt pr::Pair<String Pair<String String>> ->
        seqStmt(s1,
          if pr.snd.fst == pr.snd.snd
          then
            nullStmt()
          else
            parseStmt(
              s"struct tensor_${pr.fst} ${pr.snd.fst} = ${pr.snd.snd};"
            )
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

  forwards to
    mkErrorCheck(
      lErrors,
      substExpr(
        stmtSubstitution("__tensor_decl", tensorDecl) ::
        stmtSubstitution("__tensor_sub", tensorNameSub) ::
        stmtSubstitution("__expr_decl", exprDecl) ::
        stmtSubstitution("__tensor_prep", tensorValDec) ::
        stmtSubstitution("__check_dims", checkDims) ::
        stmtSubstitution("__tensor_pack", tensorPack) ::
        stmtSubstitution("__compute", computeStmt) ::
        []
        ,
        eqExpr(
          output,
          parseExpr(s"""
          ({
            __tensor_decl;
            __tensor_sub;
            __expr_decl;
            __tensor_pack;
            __tensor_prep;
            __check_dims;
            double t${head(access)}0 = 0.0;
            __compute;
            ${setFormats(exprToString(exNew, fmts), ex.tensors)};
            t${head(access)}0;
          })
          """),
          location=top.location
        )
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
