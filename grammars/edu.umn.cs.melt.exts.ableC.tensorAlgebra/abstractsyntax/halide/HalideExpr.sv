grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production halideSetup
top::Stmt ::= tensor::Expr idx::Expr value::Expr
{
  propagate substituted;
  top.pp = text("// Halide Tensor Expression Setup");
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
    tensorAccess(tensor, tensor, idx, top.env, location=tensor.location);
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

  local accessCalc :: [String] =
    map(
      \ e::TensorExpr ->
        let acc::[String] =
          head((decorate e with {fmts=fmts;}).accesses)
        in
        foldl(
          \ res::String v::String ->
            if length(res) == 0
            then v
            else s"((${res}) * ${v}_dimension) + ${v}"
          ,
          "",
          acc
        )
        end
      ,
      tensors
    );

  local accesses :: tm:Map<String String> =
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
    foldl(
      \ iter::IterStmt p::Pair<String Maybe<TensorExpr>> ->
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
                parseStmt(s"""
                  ${outNew.tensorName}_data[${outAcc}] += ${denseExprEval(p.snd, fmts, accesses)};
                """)
              )
            )
          else
            nullIterStmt()
        )
      ,
      nullIterStmt(),
      innerVars
    );

  local outAcc::String = 
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
        stmtIterStmt(
          parseStmt(s"${outNew.tensorName}_data[${outAcc}] = 0.0;")
        )
        ,
        seqIterStmt(
          innerLoops,
          if topExpr.isJust
          then
            stmtIterStmt(
              parseStmt(s"""
                ${outNew.tensorName}_data[${outAcc}] += ${denseExprEval(topExpr, fmts, accesses)};
              """)
            )
          else
            nullIterStmt()
        )
      )
    );

  forwards to
    stmtIterStmt(
      if !null(localErrors)
      then warnStmt(localErrors)
      else 
        fwrd
    );
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
      | tensorAccess(_, _, _, en) ->
        nothing()
      | tensorAdd(_, l, r, en) ->
        if l.isAvail
        then just(l)
        else if r.isAvail
        then just(r)
        else nothing()
      | tensorSub(e, l, r, en) ->
        if l.isAvail
        then just(l)
        else if r.isAvail
        then
          just(
            tensorSub(
              e,
              nullTensorExpr(en, location=ex.location),
              r,
              en,
              location=ex.location
            )
          )
        else nothing()
      | tensorMul(e, l, r, en) ->
        nothing()
      | tensorDiv(e, l, r, en) ->
        nothing()
      end;
}

function denseExprEval
String ::= 
  e::Maybe<TensorExpr> fmts::tm:Map<String TensorFormat> 
  acc::tm:Map<String String>
{
  return
    if e.isJust
    then
      case e.fromJust of
      | tensorBaseExpr(_, _) ->
        e.fromJust.exprName
      | tensorAccess(_, _, _, _) ->
        e.fromJust.tensorName ++ "_data[" ++
        head(tm:lookup(e.fromJust.tensorName, acc)) ++ "]"
      | tensorAdd(_, l, r, _) ->
        "(" ++ denseExprEval(just(l), fmts, acc) ++ "+" ++
        denseExprEval(just(r), fmts, acc) ++ ")"
      | tensorSub(_, l, r, _) ->
        "(" ++ denseExprEval(just(l), fmts, acc) ++ "-" ++
        denseExprEval(just(r), fmts, acc) ++ ")"
      | tensorMul(_, l, r, _) ->
        "(" ++ denseExprEval(just(l), fmts, acc) ++ "*" ++
        denseExprEval(just(r), fmts, acc) ++ ")"
      | tensorDiv(_, l, r, _) ->
        "(" ++ denseExprEval(just(l), fmts, acc) ++ "/" ++
        denseExprEval(just(r), fmts, acc) ++ ")"
      end
    else "";
}
