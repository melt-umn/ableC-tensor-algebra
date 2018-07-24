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

  local leftOnly::[String] =
    let lAcc::[String] =
      nubBy(
        stringEq,
        flatMap(\l::[String] -> l, out.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        flatMap(\l::[String] -> l, ex.accesses)
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

  local out::TensorExpr =
    tensorAccess(tensor, tensor, idx, top.env, location=top.location);
  
  local ex::TensorExpr =
    right.tensorExp;

  local order::Maybe<[String]> =
    mergeOrder(out.accesses ++ ex.accesses);

  local access::[String] =
    case order of
    | nothing() -> []
    | just(l) -> l
    end;

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      map(
        \ e::TensorExpr ->
          pair(getTensorName(e), getTensorFormat(e))
        ,
        ex.tensors ++ out.tensors
      )
      ,
      tm:empty(compareString)
    );

  ex.accessOrder = access;

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
      out, fmts, ex, access,
      top.location, top.env
    );

  local computeStmt::Stmt =
    parseStmt(
      graph.compute
    );

  local fmtNm::String =
    getTensorFormat(out).proceduralName;

  local outOrder::Integer = 
    getTensorFormat(out).dimensions;

  local assembleStmt::Stmt =
    parseStmt(
      s"unsigned long* idx = GC_malloc(sizeof(unsigned long) * ${toString(listLength(head(out.accesses)))});\n"
      ++
      s"struct tensor_${fmtNm}* t = &${out.tensorName};\n"
      ++
      s"t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(outOrder)});\n"
      ++
      "unsigned long count = 1;\n"
      ++
      generateMakeBody(getTensorFormat(out).storage)
      ++
      graph.asmbl
      ++
      s"""
      unsigned long* dims = ${out.tensorName}.dims;
      tensor_packTree_${fmtNm}(&(${out.tensorName}.buffer), dims);
      
      struct tensor_tree_s* buffer = &(${out.tensorName}.buffer);
      t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(outOrder)});
      unsigned long numChildren = 1;
      struct tensor_tree_s** trees = &buffer;

      struct tensor_tree_s** temp_tree;
      unsigned long total, dimSize, index, newChildren;

      ${generatePackBody_Assemble(getTensorFormat(out).storage)}

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
              getTensorFormat(e)
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
      ex.tensors ++ out.tensors
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
      ex.exprs
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
          generateTensorVals(e)
        )
      ,
      ex.tensors
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
          generateTensorVals(e)
        )
      ,
      out.tensors
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
      check_dims(out, ex, access, fmts)
    );

  local tensorPack :: Stmt =
    foldl(
      \ s1::Stmt e::TensorExpr ->
        seqStmt(s1,
          let fmt::String =
            getTensorFormat(e).proceduralName
          in
          parseStmt(
            s"tensor_pack_${fmt}(&${e.tensorName});"
          )
          end
        )
      ,
      nullStmt(),
      ex.tensors
    );

  computeStmt.env = top.env;
  computeStmt.returnType = nothing();

  forwards to
    mkErrorCheck(
      lErrors,
      substExpr(
        stmtSubstitution("__tensor_decl", tensorDecl) ::
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
          __expr_decl;
          __tensor_pack;
          __tensor_prep;
          __check_dims;
          if(${checkFormats(exprToString(ex), ex.tensors ++ out.tensors)}) {
            {__assemble;}
          }
          __out_prep;
          __compute;
          ${setFormats(exprToString(ex), ex.tensors ++ out.tensors)}
          ${out.tensorName};
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

  local order::Maybe<[String]> =
    mergeOrder(ex.accesses);

  local access::[String] =
    case order of
    | nothing() -> []
    | just(l) -> l
    end;

  local lErrors::[Message] =
    case order of
    | nothing() -> [err(top.location, "Cannot generate code for this tensor expression due to cyclical access patterns")]
    | just(_) -> []
    end;

  forwards to
    mkErrorCheck(
      lErrors,
      mkStringConst(implode(", ", access), top.location)
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
    if null(acc) || null(tail(acc))
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
