grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Generate the Halide Stmt for a tensor expression 
   with the lhs being a scalar. -}
abstract production halideScalarTensorExpr
top::Stmt ::= output::Name expr::Expr
{
  top.pp = expr.pp;
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

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
        getTensorFormat(e, tm:empty())
      ,
      tensors
    );

  {- For each tensor, figure out the expression that we use to
     access elements from the tensor. -}
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
      tm:empty()
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

  local exNew::TensorExpr =
    modifyNames(
      newNames, 
      ex
    );
  
  out.fmts = fmts;
  ex.fmts = fmts;
  exNew.fmts = fmts;

  -- Automatically determine the merge order (if possible)
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
      tm:empty()
    );

  -- Check that all tensors are dense tensors
  local allDense::[Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) -> !contains(storeSparse, specs)
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

  {- For each dimension, figure out what part of the expression can 
     be calculated at that level -}
  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    mapWithTail(
      \ v::String rm::[String] ->
        pair(v, denseReduce(exNew, v, rm, fmts))
      ,
      access
    );

  {- Build all the loops and output any available expressions -}
  local innerLoops :: Stmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::Stmt ->
        multiForStmt(
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
            seqStmt(
              iter,
              ableC_Stmt {
                __result += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
              }
            )
          else
            iter
        )
      ,
      nullStmt(),
      innerVars
    );

  forwards to
    if !null(localErrors)
    then warnStmt(localErrors)
    else innerLoops;
}

{- Production for scalar valued Halide tensor expression, if
   the order loops construct is used to specify the order of 
   the loops to use -}
abstract production halideScalarExprOrder
top::Stmt ::= output::Name expr::Expr access::[String]
{
  top.pp = expr.pp;
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

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
      getTensorFormat(_, tm:empty()),
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
      tm:empty()
    );

  local newNames :: [String] =
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

  out.fmts = fmts;
  ex.fmts = fmts;
  exNew.fmts = fmts;

  local allVars :: [String] = nub(concat(exNew.accesses));

  {- Check that all variables in the equation are in 
     the provided order, and no extra variables are 
     added -}
  local missingVar :: Boolean =
    !containsAll(allVars, access)
    ||
    !containsAll(access, allVars);

  local fmts :: tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty()
    );

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) -> !contains(storeSparse, specs)
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

  local innerLoops :: Stmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::Stmt ->
        multiForStmt(
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
            seqStmt(
              iter,
              ableC_Stmt {
                __result += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
              }
            )
          else
            iter
        )
      ,
      nullStmt(),
      innerVars
    );

  forwards to
    if !null(localErrors)
    then warnStmt(localErrors)
    else innerLoops;
}

{- Production for Stmt of a Halide tensor expression without
   a provided order for the loops, and where the lhs is not a scalar -}
abstract production halideTensorExpr
top::Stmt ::= tensor::Expr idx::Expr value::Expr
{
  top.pp = 
    ppConcat([
      tensor.pp,
      text("["),
      idx.pp,
      text("] = "),
      value.pp
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;
    
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
        getTensorFormat(e, tm:empty())
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
      tm:empty()
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

  {- Check for any indexvars appearing on only the lhs.
     (This is not allowed) -}
  local leftOnly::[String] =
    let lAcc::[String] = nub(concat(outNew.accesses))
    in
    let rAcc::[String] = nub(concat(exNew.accesses))
    in
    filter(
      \ v::String -> !contains(v, rAcc)
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
      tm:empty()
    );

  local allDense::[Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) -> !contains(storeSparse, specs)
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

  {- Determine all loops before we can access elements
     in the output. Until we reach this point, we cannot
     put values into the output tensor, so we emit them
     all together -}
  local topVars :: [String] =
    let i::Integer = positionOf(last(head(out.accesses)), access)
    in
    take(i+1, access)
    end;

  {- If there's any expression that can be emitted inside 
     the final loop that accesses the output tensor -}
  local topExpr :: Maybe<TensorExpr> =
    let i::Integer = positionOf(last(head(out.accesses)), access)
    in
    denseReduce(
      exNew,
      last(head(out.accesses)),
      drop(i+1, access),
      fmts
    )
    end;

  {- Variables below the last indexvar that accesses the
     output tensor -}
  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    let i::Integer =  positionOf(last(head(out.accesses)), access)
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

  local innerLoops :: Stmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::Stmt ->
        multiForStmt(
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
            seqStmt(
              iter,
              ableC_Stmt {
                $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
              }
            )
          else
            iter
        )
      ,
      nullStmt(),
      innerVars
    );

  local outAcc::Expr = 
    getElem(accessCalc, positionOf(outNew.tensorName, newNames)).fromJust;

  local fwrd :: Stmt =
    multiForStmt(
      topLoop,
      seqStmt(
        innerLoops,
        if topExpr.isJust
        then
          ableC_Stmt {
            $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(topExpr.fromJust, fmts, accesses)};
          }
        else
          nullStmt()
      )
    );

  forwards to
    if !null(localErrors)
    then warnStmt(localErrors)
    else fwrd;
}

{- Stmt production for tensor expression with an order provided -}
abstract production halideTensorExprOrder
top::Stmt ::= tensor::Expr idx::Expr value::Expr access::[String]
{
  top.pp =
    ppConcat([
      tensor.pp,
      text("["),
      idx.pp,
      text("] = "),
      value.pp
    ]);
  top.functionDefs := [];
  top.labelDefs := [];

  propagate controlStmtContext, env;

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
      getTensorFormat(_, tm:empty()),
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
      tm:empty()
    );

  local newNames :: [String] =
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
    let lAcc::[String] =  nub(concat(outNew.accesses))
    in
    let rAcc::[String] = nub(concat(exNew.accesses))
    in
    filter(
      \ v::String -> !contains(v, rAcc)
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

  local allVars :: [String] = nub(concat(outNew.accesses ++ exNew.accesses));

  local missingVar :: Boolean =
    !containsAll(allVars, access) || !containsAll(access, allVars);

  local fmts::tm:Map<String TensorFormat> =
    tm:add(
      zipWith(
        pair,
        newNames,
        tensorFormats
      ),
      tm:empty()
    );

  local allDense :: [Boolean] =
    map(
      \ fmt::TensorFormat ->
        case fmt of
        | tensorFormat(specs, _, _) -> !contains(storeSparse, specs)
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
   let i::Integer = lastIndexOf(head(out.accesses), access)
    in
    take(i+1, access)
    end;

  local topExpr :: Maybe<TensorExpr> =
    let i::Integer = lastIndexOf(head(out.accesses), access)
    in
    denseReduce(
      exNew,
      getElem(access, i).fromJust,
      drop(i+1, access),
      fmts
    )
    end;

  local innerVars :: [Pair<String Maybe<TensorExpr>>] =
    let i::Integer = lastIndexOf(head(out.accesses), access)
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

  local innerLoops :: Stmt =
    foldr(
      \ p::Pair<String Maybe<TensorExpr>> iter::Stmt ->
        multiForStmt(
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
            seqStmt(
              iter,
              ableC_Stmt {
                $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(p.snd.fromJust, fmts, accesses)};
              }
            )
          else
            iter
        )
      ,
      nullStmt(),
      innerVars
    );

  local outAcc::Expr =
    getElem(accessCalc, positionOf(outNew.tensorName, newNames)).fromJust;

  local fwrd::Stmt =
    multiForStmt(
      topLoop,
      seqStmt(
        innerLoops,
        if topExpr.isJust
        then
          ableC_Stmt {
            $name{s"${outNew.tensorName}_data"}[$Expr{outAcc}] += $Expr{denseEvalExpr(topExpr.fromJust, fmts, accesses)};
          }
        else
          nullStmt()
      )
    );

  forwards to
    if !null(localErrors)
    then warnStmt(localErrors)
    else fwrd;
}

{- Determine if any part of the TensorExpr can be 
   emitted at the current var with remain index
   vars left to be looped over. -}
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
      | tensorAdd(l, r, en) -> -- need just one side avail
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
              nullTensorExpr(en, location=ex.location), -- the value 0
              r,
              en,
              location=ex.location
            )
          )
        else nothing()
      | tensorMul(l, r, en) -> -- need both side avail
        nothing()
      | tensorDiv(l, r, en) ->
        nothing()
      end;
}

{- Evaluate the TensorExpr into an ableC Expr. The only
   difference from a normal evalExpr is that we use the
   previously calculated access Expr's to access the data
   field. -}
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
