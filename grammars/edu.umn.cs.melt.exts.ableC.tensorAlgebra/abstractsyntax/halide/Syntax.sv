grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production halideTensorCompute
top::Stmt ::= tns::Expr idx::Expr val::Expr ts::Transformation
{
  propagate substituted;
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

  local out::TensorExpr =
    tensorAccess(tns, tns, idx, top.env, location=tns.location);
  local ex::TensorExpr =
    val.tensorExp;

  out.fmts = tm:empty(compareString);
  ex.fmts = tm:empty(compareString);

  local tensors::[TensorExpr] = 
    ex.tensors ++ out.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local leftOnly::[String] =
    let lAcc::[String] =
      nubBy(
        stringEq,
        concat(out.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        concat(ex.accesses)
      )
    in
    filter(
      \ v::String -> !containsBy(stringEq, v, rAcc)
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

  local lDefs :: [Def] =
    map(
      \ v::String ->
        valueDef(v, 
          declaratorValueItem(
            decorate
              declarator(
                name(v, location=tns.location),
                baseTypeExpr(),
                nilAttribute(),
                nothingInitializer()
              )
            with {
              typeModifiersIn=[];
              returnType=nothing();
              isTypedef=false;
              isTopLevel=false;
              givenAttributes=nilAttribute();
              baseType=builtinType(nilQualifier(), unsignedType(longType()));
              env=top.env;
            }
          )
        )
      ,
      access
    ); 

  local sErrors :: [Message] =
    checkTensorHeader(tns.location, top.env)
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
      then [err(tns.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(",", leftOnly)} only occur on the left-hand side.")]
      else 
        case order of
        | nothing() -> [err(tns.location, s"Cannot generate code for this tensor expression due to cyclical access pattern. Specify \"order loops\" to fix.")]
        | _ -> 
          map(
            \ msg::Message -> 
              case msg of
              | err(l, s) -> 
                let lst::[String] =
                  explode(" ", s)
                in
                err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
                end
              | _ -> msg
              end
            ,
            (decorate ts with {env=top.env;iterStmtIn=nullIterStmtDefs(lDefs);returnType=nothing();}).errors
          )
        end
    else [];

  local loops :: IterStmt =
    halideTensorExpr(tns, idx, val);

  local setup :: (Stmt ::= Stmt) =
    halideSetup(tns, idx, val, _);

  local fwrd::Stmt =
    setup(
      iterateStmt(loops, ts)
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
  propagate substituted;
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

  local out::TensorExpr =
    tensorAccess(tns, tns, idx, top.env, location=tns.location);
  local ex::TensorExpr =
    val.tensorExp;

  out.fmts = tm:empty(compareString);
  ex.fmts = tm:empty(compareString);

  local tensors::[TensorExpr] = 
    ex.tensors ++ out.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local leftOnly::[String] =
    let lAcc::[String] =
      nubBy(
        stringEq,
        concat(out.accesses)
      )
    in
    let rAcc::[String] =
      nubBy(
        stringEq,
        concat(ex.accesses)
      )
    in
    filter(
      \ v::String -> !containsBy(stringEq, v, rAcc)
      ,
      lAcc
    )
    end end;

  local invalidLeftVar :: Boolean =
    !null(leftOnly);

  local allVars :: [String] =
    nubBy(
      stringEq,
      concat(out.accesses ++ ex.accesses)
    );

  local missingVar :: Boolean =
    !containsAll(stringEq, allVars, ord)
    ||
    !containsAll(stringEq, ord, allVars);

  local lDefs :: [Def] =
    map(
      \ v::String ->
        valueDef(v, 
          declaratorValueItem(
            decorate
              declarator(
                name(v, location=tns.location),
                baseTypeExpr(),
                nilAttribute(),
                nothingInitializer()
              )
            with {
              typeModifiersIn=[];
              returnType=nothing();
              isTypedef=false;
              isTopLevel=false;
              givenAttributes=nilAttribute();
              baseType=builtinType(nilQualifier(), unsignedType(longType()));
              env=top.env;
            }
          )
        )
      ,
      ord
    ); 

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

  local sErrors :: [Message] =
    checkTensorHeader(tns.location, top.env)
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
      then [err(tns.location, s"Cannot generate code for this tensor expression because the variable(s) ${implode(",", leftOnly)} only occur on the left-hand side.")]
      else
        if missingVar
        then [err(tns.location, s"Specified order for the loops cannot be used, as some dimensions are missing.")]
        else
          map(
            \ msg::Message -> 
              case msg of
              | err(l, s) -> 
                if endsWith("is not a transformable loop", s)
                then
                  let lst::[String] =
                    explode(" ", s)
                  in
                  err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
                  end
                else msg
              | _ -> msg
              end
            ,
            (decorate ts with {env=top.env;iterStmtIn=nullIterStmtDefs(lDefs);returnType=nothing();}).errors
          )
    else [];

  local loops :: IterStmt =
    halideTensorExprOrder(tns, idx, val, ord);

  local setup :: (Stmt ::= Stmt) =
    halideSetup(tns, idx, val, _);

  local fwrd :: Stmt =
    setup(iterateStmt(loops, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production halideScalarCompute
top::Stmt ::= nm::Name val::Expr ts::Transformation
{
  propagate substituted;
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

  local ex::TensorExpr =
    val.tensorExp;

  ex.fmts = tm:empty(compareString);

  local tensors::[TensorExpr] = 
    ex.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local order::Maybe<[String]> =
    mergeOrder(ex.accesses);

  local access::[String] =
    order.fromJust;

  local lDefs :: [Def] =
    map(
      \ v::String ->
        valueDef(v, 
          declaratorValueItem(
            decorate
              declarator(
                name(v, location=nm.location),
                baseTypeExpr(),
                nilAttribute(),
                nothingInitializer()
              )
            with {
              typeModifiersIn=[];
              returnType=nothing();
              isTypedef=false;
              isTopLevel=false;
              givenAttributes=nilAttribute();
              baseType=builtinType(nilQualifier(), unsignedType(longType()));
              env=top.env;
            }
          )
        )
      ,
      access
    ); 

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

  local sErrors :: [Message] =
    checkTensorHeader(nm.location, top.env)
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
    val.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then
      case order of
      | nothing() -> [err(nm.location, s"Cannot generate code for this tensor expression due to cyclical access pattern. Specify \"order loops\" to fix.")]
      | _ -> 
        map(
          \ msg::Message -> 
            case msg of
            | err(l, s) -> 
              let lst::[String] =
                explode(" ", s)
              in
              err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
              end
            | _ -> msg
            end
          ,
          (decorate ts with {env=top.env;iterStmtIn=nullIterStmtDefs(lDefs);returnType=nothing();}).errors
        )
      end
    else [];
  
  local loops :: IterStmt =
    halideScalarTensorExpr(nm, val);

  local setup :: (Stmt ::= Stmt) =
    halideScalarSetup(nm, val, _);

  local fwrd :: Stmt =
    setup(iterateStmt(loops, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production halideScalarComputeOrdered
top::Stmt ::= nm::Name val::Expr ord::[String] ts::Transformation
{
  propagate substituted;
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

  local ex::TensorExpr =
    val.tensorExp;

  ex.fmts = tm:empty(compareString);

  local tensors::[TensorExpr] = 
    ex.tensors;

  local tensorFormats::[TensorFormat] =
    map(
      getTensorFormat(_, tm:empty(compareString)),
      tensors
    );

  local allVars :: [String] =
    nubBy(
      stringEq,
      concat(ex.accesses)
    );

  local missingVar :: Boolean =
    !containsAll(stringEq, allVars, ord)
    ||
    !containsAll(stringEq, ord, allVars);


  local lDefs :: [Def] =
    map(
      \ v::String ->
        valueDef(v,
          declaratorValueItem(
            decorate
              declarator(
                name(v, location=nm.location),
                baseTypeExpr(),
                nilAttribute(),
                nothingInitializer()
              )
            with {
              typeModifiersIn=[];
              returnType=nothing();
              isTypedef=false;
              isTopLevel=false;
              givenAttributes=nilAttribute();
              baseType=builtinType(nilQualifier(), unsignedType(longType()));
              env=top.env;
            }
          )
        )
      ,
      ord
    ); 

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

  local sErrors :: [Message] =
    checkTensorHeader(nm.location, top.env)
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
    val.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then
      if missingVar
      then [err(nm.location, s"Specified order for the loops cannot be used, as some dimensions are missing.")]
      else
        map(
          \ msg::Message -> 
            case msg of
            | err(l, s) -> 
              let lst::[String] =
                explode(" ", s)
              in
              err(l, s"Cannot transform ${head(lst)} because the index variable is not used in the tensor exprssion")
              end
            | _ -> msg
            end
          ,
          (decorate ts with {env=top.env;iterStmtIn=nullIterStmtDefs(lDefs);returnType=nothing();}).errors
        )
    else [];

  local loops :: IterStmt =
    halideScalarExprOrder(nm, val, ord);

  local setup :: (Stmt ::= Stmt) =
    halideScalarSetup(nm, val, _);

  local fwrd :: Stmt =
    setup(iterateStmt(loops, ts));
  fwrd.env = top.env;

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

-- A production for producing an empty iterStmt, but injecting defs
-- into it. This is used to generate transformation errors without 
-- errors from the code itself. (Such as undeclared values, since
-- those values were declared in other code produced by the production)
abstract production nullIterStmtDefs
top::IterStmt ::= defs::[Def]
{
  top.iterDefs = defs;
  forwards to nullIterStmt();
}
