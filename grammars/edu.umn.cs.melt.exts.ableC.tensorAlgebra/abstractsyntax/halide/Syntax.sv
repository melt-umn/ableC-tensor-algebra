grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production halideTensorCompute
top::Stmt ::= tns::Expr idx::Expr val::Expr ts::Transformation
{
  top.pp = ppConcat([
      text("tensor transform {\n"),
      tns.pp,
      text("["),
      idx.pp,
      text("] = "),
      val.pp,
      text(";\n} by {\n"),
      ts.pp,
      text("}")
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

  local out::TensorExpr =
    tensorAccess(tns, idx, top.env);
  local ex::TensorExpr =
    val.tensorExp;

  out.fmts = tm:empty();
  ex.fmts = tm:empty();

  local tensors::[TensorExpr] = 
    ex.tensors ++ out.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty()),
      tensors
    );

  local leftOnly::[String] =
    let lAcc::[String] = nub(concat(out.accesses))
    in
    let rAcc::[String] = nub(concat(ex.accesses))
    in
    filter(
      \ v::String -> !contains(v, rAcc)
      ,
      lAcc
    )
    end end;

  local invalidLeftVar :: Boolean =
    !null(leftOnly);

  local order::Maybe<[String]> =
    mergeOrder(out.accesses ++ ex.accesses);

  local access::[String] =
    order.fromJust;

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _) -> !contains(storeSparse, specs)
        | _ -> false
        end
      ,
      tensorFormats
    );

  local topVars :: [String] =
    let i :: Integer = positionOf(last(head(out.accesses)), access)
    in
    take(i+1, access)
    end;

  local innerVars :: [String] =
    let i :: Integer = positionOf(last(head(out.accesses)), access)
    in
    drop(i+1, access)
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
          name(s),
          declRefExpr(
            name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
          ),
          var
        )
      ,
      nilIterVar(),
      topVars
    );

  local innerLoops :: Stmt =
    foldr(
      \ v::String iter::Stmt ->
        multiForStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(v),
            declRefExpr(
              name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
            ),
            nilIterVar()
          ),
          iter
        )
      ,
      nullStmt(),
      innerVars
    );

  local loops :: Stmt =
    multiForStmt(
      topLoop,
      innerLoops
    );

  local sErrors :: [Message] =
    checkTensorHeader(top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else errFromOrigin(fmt.fst, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zip(tensors, allDense)
    )
    ++
    tns.errors
    ++
    idx.errors
    ++
    val.errors;

  local lErrors :: [Message] =
    sErrors 
    ++
    if null(sErrors)
    then
      if invalidLeftVar
      then [errFromOrigin(tns, s"Cannot generate code for this tensor expression because the variable(s) ${implode(",", leftOnly)} only occur on the left-hand side.")]
      else 
        case order of
        | nothing() -> [errFromOrigin(tns, s"Cannot generate code for this tensor expression due to cyclical access pattern. Specify \"order loops\" to fix.")]
        | _ -> 
          map(
            \ msg::Message -> 
              case msg of
              | err(l, s) ->
                let lst::[String] =
                  explode(" ", s)
                in
                if endsWith("is not a transformable loop", s)
                then
                  err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
                else if startsWith("Loop", s) && endsWith("is not contiguous", s)
                then
                  err(l, s"Cannot perform transformation because ${head(tail(lst))} is accessed after the final dimension of the output tensor. If desired, use \"order loops\" to move ${head(tail(lst))} above this.")
                else if startsWith("Duplicate loop name", s)
                then 
                  err(l, s"Duplicate index variable ${last(lst)}")
                else msg
                end
              | _ -> msg
              end
            ,
            (decorate ts with {env=top.env;iterStmtIn=stmtIterStmt(loops);
                controlStmtContext = initialControlStmtContext;}).errors
          )
        end
    else [];

  local body :: Stmt =
    halideTensorExpr(tns, idx, val);

  local setup :: (Stmt ::= Stmt) =
    halideSetup(tns, idx, val, _);

  local fwrd::Stmt =
    setup(
      transformStmt(body, ts)
    );
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production halideTensorComputeOrdered
top::Stmt ::= tns::Expr idx::Expr val::Expr ord::[String] ts::Transformation
{
  top.pp = ppConcat([
      text("tensor transform {\n"),
      tns.pp,
      text("["),
      idx.pp,
      text("] = "),
      val.pp,
      text(";\n} by {\n"),
      text(s"order loops ${implode(",", ord)};\n"),
      ts.pp,
      text("}")
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

  local out::TensorExpr =
    tensorAccess(tns, idx, top.env);
  local ex::TensorExpr =
    val.tensorExp;

  out.fmts = tm:empty();
  ex.fmts = tm:empty();

  local tensors::[TensorExpr] = 
    ex.tensors ++ out.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty()),
      tensors
    );

  local leftOnly::[String] =
    let lAcc::[String] = nub(concat(out.accesses))
    in
    let rAcc::[String] = nub(concat(ex.accesses))
    in
    filter(
      \ v::String -> !contains(v, rAcc)
      ,
      lAcc
    )
    end end;

  local invalidLeftVar :: Boolean =
    !null(leftOnly);

  local allVars :: [String] = nub(concat(out.accesses ++ ex.accesses));

  local missingVar :: Boolean =
    !containsAll(allVars, ord)
    ||
    !containsAll(ord, allVars);

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _) -> !contains(storeSparse, specs)
        | _ -> false
        end
      ,
      tensorFormats
    );

  local topVars :: [String] =
    let i :: Integer = lastIndexOf(head(out.accesses), ord )
    in
    take(i+1, ord)
    end;

  local innerVars :: [String] =
    let i :: Integer = lastIndexOf(head(out.accesses), ord)
    in
    drop(i+1, ord)
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
          name(s),
          declRefExpr(
            name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
          ),
          var
        )
      ,
      nilIterVar(),
      topVars
    );

  local innerLoops :: Stmt =
    foldr(
      \ v::String iter::Stmt ->
        multiForStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(v),
            declRefExpr(
              name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
            ),
            nilIterVar()
          ),
          iter
        )
      ,
      nullStmt(),
      innerVars
    );

  local loops :: Stmt =
    multiForStmt(
      topLoop,
      innerLoops
    );

  local sErrors :: [Message] =
    checkTensorHeader(top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else errFromOrigin(fmt.fst, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zip(tensors, allDense)
    )
    ++
    tns.errors
    ++
    idx.errors
    ++
    val.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then
      if invalidLeftVar
      then [errFromOrigin(tns, s"Cannot generate code for this tensor expression because the variable(s) ${implode(",", leftOnly)} only occur on the left-hand side.")]
      else
        if missingVar
        then [errFromOrigin(tns, s"Specified order for the loops cannot be used, as some dimensions are missing.")]
        else
          map(
            \ msg::Message -> 
              case msg of
              | err(l, s) ->
                let lst::[String] =
                  explode(" ", s)
                in
                if endsWith("is not a transformable loop", s)
                then
                  err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
                else if startsWith("Loop", s) && endsWith("is not contiguous", s)
                then
                  err(l, s"Cannot perform transformation because ${head(tail(lst))} is accessed after the final dimension of the output tensor. If desired, use \"order loops\" to move ${head(tail(lst))} above this.")
                else if startsWith("Duplicate loop name", s)
                then 
                  err(l, s"Duplicate index variable ${last(lst)}")
                else msg
                end
              | _ -> msg
              end
            ,
            (decorate ts with {env=top.env;iterStmtIn=stmtIterStmt(loops);
                controlStmtContext = initialControlStmtContext;}).errors
          )
    else [];

  local body :: Stmt =
    halideTensorExprOrder(tns, idx, val, ord);

  local setup :: (Stmt ::= Stmt) =
    halideSetup(tns, idx, val, _);

  local fwrd :: Stmt =
    setup(transformStmt(body, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production halideScalarCompute
top::Stmt ::= nm::Name val::Expr ts::Transformation
{
  top.pp = ppConcat([
      text("tensor transform {\n"),
      nm.pp,
      text(" = "),
      val.pp,
      text(";\n} by {\n"),
      ts.pp,
      text("}")
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

  local ex::TensorExpr =
    val.tensorExp;

  ex.fmts = tm:empty();

  local tensors::[TensorExpr] = 
    ex.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty()),
      tensors
    );

  local order::Maybe<[String]> =
    mergeOrder(ex.accesses);

  local access::[String] =
    order.fromJust;

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _) -> !contains(storeSparse, specs)
        | _ -> false
        end
      ,
      tensorFormats
    );

  local loops :: Stmt =
    foldr(
      \ v::String iter::Stmt ->
        multiForStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(v),
            declRefExpr(
              name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
            ),
            nilIterVar()
          ),
          iter
        )
      ,
      nullStmt(),
      access
    );

  local sErrors :: [Message] =
    checkTensorHeader(top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else errFromOrigin(fmt.fst, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zip(tensors, allDense)
    )
    ++
    val.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then
      case order of
      | nothing() -> [errFromOrigin(nm, s"Cannot generate code for this tensor expression due to cyclical access pattern. Specify \"order loops\" to fix.")]
      | _ -> 
        map(
          \ msg::Message -> 
            case msg of
            | err(l, s) ->
              let lst::[String] =
                explode(" ", s)
              in
              if endsWith("is not a transformable loop", s)
              then
                err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
              else if startsWith("Loop", s) && endsWith("is not contiguous", s)
              then
                err(l, s"Cannot perform transformation because ${head(tail(lst))} is accessed after the final dimension of the output tensor. If desired, use \"order loops\" to move ${head(tail(lst))} above this.")
              else if startsWith("Duplicate loop name", s)
              then 
                err(l, s"Duplicate index variable ${last(lst)}")
              else msg
              end
            | _ -> msg
            end
          ,
          (decorate ts with {env=top.env;iterStmtIn=stmtIterStmt(loops);
                controlStmtContext = initialControlStmtContext;}).errors
        )
      end
    else [];
  
  local body :: Stmt =
    halideScalarTensorExpr(nm, val);

  local setup :: (Stmt ::= Stmt) =
    halideScalarSetup(nm, val, _);

  local fwrd :: Stmt =
    setup(transformStmt(body, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production halideScalarComputeOrdered
top::Stmt ::= nm::Name val::Expr ord::[String] ts::Transformation
{
  top.pp = ppConcat([
      text("tensor transform {\n"),
      nm.pp,
      text(" = "),
      val.pp,
      text(";\n} by {\n"),
      text(s"order loops ${implode(",", ord)};\n"),
      ts.pp,
      text("}")
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

  local ex::TensorExpr =
    val.tensorExp;

  ex.fmts = tm:empty();

  local tensors::[TensorExpr] = 
    ex.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty()),
      tensors
    );

  local allVars :: [String] = nub(concat(ex.accesses));

  local missingVar :: Boolean =
    !containsAll(allVars, ord)
    ||
    !containsAll(ord, allVars);

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _) -> !contains(storeSparse, specs)
        | _ -> false
        end
      ,
      tensorFormats
    );

  local loops :: Stmt =
    foldr(
      \ v::String iter::Stmt ->
        multiForStmt(
          consIterVar(
            builtinTypeExpr(
              nilQualifier(),
              unsignedType(
                longType()
              )
            ),
            baseTypeExpr(),
            name(v),
            declRefExpr(
              name("edu_umn_cs_melt_exts_ableC_tensorAlgebra")
            ),
            nilIterVar()
          ),
          iter
        )
      ,
      nullStmt(),
      ord
    );

  local sErrors :: [Message] =
    checkTensorHeader(top.env)
    ++
    foldl(
      \ lst::[Message] fmt::Pair<TensorExpr Boolean> ->
        if fmt.snd
        then lst
        else errFromOrigin(fmt.fst, s"Tensor ${getTensorName(fmt.fst)} has sparse dimensions. Halide transforming is only supported on equations with only dense tensors.") :: lst
      ,
      [],
      zip(tensors, allDense)
    )
    ++
    val.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then
      if missingVar
      then [errFromOrigin(nm, s"Specified order for the loops cannot be used, as some dimensions are missing.")]
      else
        map(
          \ msg::Message -> 
            case msg of
            | err(l, s) ->
              let lst::[String] =
                explode(" ", s)
              in
              if endsWith("is not a transformable loop", s)
              then
                err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
              else if startsWith("Loop", s) && endsWith("is not contiguous", s)
              then
                err(l, s"Cannot perform transformation because ${head(tail(lst))} is accessed after the final dimension of the output tensor. If desired, use \"order loops\" to move ${head(tail(lst))} above this.")
              else if startsWith("Duplicate loop name", s)
              then 
                err(l, s"Duplicate index variable ${last(lst)}")
              else msg
              end
            | _ -> msg
            end
          ,
          (decorate ts with {env=top.env;iterStmtIn=stmtIterStmt(loops);
                controlStmtContext = initialControlStmtContext;}).errors
        )
    else [];

  local body :: Stmt =
    halideScalarExprOrder(nm, val, ord);

  local setup :: (Stmt ::= Stmt) =
    halideScalarSetup(nm, val, _);

  local fwrd :: Stmt =
    setup(transformStmt(body, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}
