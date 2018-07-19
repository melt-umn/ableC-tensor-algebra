grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production accessTensor
top::Expr ::= tensor::Expr idx::Exprs env::Decorated Env
{
  propagate substituted;

  local fmt::TensorFormat =
    case tensor.typerep of 
    | tensorType(_, f, _) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;

  local access::[String] =
    orderList(
      getAccess(idx, env),
      map(
        \ p::Pair<Integer Pair<Integer Integer>>
        -> p.snd.fst
        ,
        fmt.storage
      )
    );
  
  local types::[Integer] =
    map(
      \ p::Pair<Integer Pair<Integer Integer>> -> p.snd.snd
      ,
      fmt.storage
    );

  local allIndexVars::Boolean =
    foldl(
      \ b::Boolean t::Type
      -> b &&
         case t of
         | indexVarType(_) -> true
         | _ -> false
         end
      ,
      true,
      idx.typereps
    )
    &&
    case idx of
    | nilExpr() -> false
    | _ -> true
    end;
  
  local anyIndexVars::Boolean =
    foldl(
      \ b::Boolean t::Type
      -> b ||
         case t of
         | indexVarType(_) -> true
         | _ -> false
         end
      ,
      false,
      idx.typereps
    );

  local indexVarErr::Boolean =
    anyIndexVars && !allIndexVars;

  local lErrors::[Message] = tensor.errors ++ idx.errors;
  
  local tErrors::[Message] =
    flatMap(
      \ t::Type
      -> t.errors
         ++
         if listLength(t.errors) != 0 || t.isIntegerType
         then []
         else [err(tensor.location, s"Expected integer type, got ${showType(t)}")]
      ,
      idx.typereps
    )
    ++
    case tensor.typerep of
    | tensorType(_, f, _) -> f.tensorFormatLookupCheck
    | x -> [err(tensor.location, s"Expected a tensor type, got ${showType(x)}")]
    end;
  
  local sErrors::[Message] =
    if idx.count != fmt.dimensions
    then [err(tensor.location, s"Number of dimensions specified does not match, expected ${toString(fmt.dimensions)}, got ${toString(idx.count)}.")]
    else [];
  
  local format::Name =
    case tensor.typerep of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=tensor.location)
    end;
  format.env = top.env;
  
  local fmtNm::String = fmt.proceduralName;
  
  top.pp = ppConcat([
             tensor.pp,
             text("("),
             ppImplode(text(", "), idx.pps),
             text(")")
           ]);

  local fwrd::Expr =
    if allIndexVars
    then
      emptyAccess
    else
      substExpr(
        declRefSubstitution("__tensor", tensor)
        :: generateExprsSubs(idx, 0),
        parseExpr(
          if top.lValue
          then 
            if fmt.dimensions == 0
            then s"""
            *({
              struct tensor_scalar* _tensor = &(__tensor);
              _tensor->data;
            })
            """
            else s"""
            *({
              struct tensor_${fmtNm}* _tensor = &(__tensor);
              unsigned long __index[] = { ${generateExprsArray(idx, 0)} };
              tensor_getPointer_${fmtNm}(_tensor, __index);
            })
            """
          else
            if fmt.dimensions == 0
            then s"""
            ({
              struct tensor_scalar* _tensor = &(__tensor);
              _tensor->data[0];
            })
            """
            else s"""
            ({
              struct tensor_${fmtNm}* _tensor = &(__tensor);
              unsigned long __index[] = { ${generateExprsArray(idx, 0)} };
              tensor_pack_${fmtNm}(_tensor);
              tensor_get_${fmtNm}(_tensor, __index);
            })
            """
        )
      );

  top.tensorName = head(top.tensorNames);

  top.conds = 
    if allIndexVars
    then
      map(
        \ s::String -> 
           let i::Integer =
             positionBy(
               \ is::String -> is == s
               ,
               access             
             )
           in
           if i == -1
           then nullCond()
           else 
             if case getElem(types, i) of
                | nothing() -> false
                | just(x) -> x == storeSparse
                end
             then sparseAccess(top, i, env, head(top.tensorNames))
             else denseAccess(top, s, i, env, head(top.tensorNames))
           end
        ,
        top.accessOrder
      )
    else
      map(
        \ s::String -> allCond()
        ,
        top.accessOrder
      );
  
  top.subExpr =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );
  
  top.sparse = 
    if allIndexVars
    then
      map(
        \ s::String ->
           let i::Integer =
             positionBy(
               \ is::String -> is == s
               ,
               access
             )
           in
           if i == -1
           then []
           else
             if case getElem(types, i) of
                | nothing() -> true
                | just(x) -> x == storeDense
                end
             then []
             else [pair(top, i)]
           end
        ,
        top.accessOrder
      )
    else
      map(
        \ s::String -> []
        ,
        top.accessOrder
      );
  
  top.canSub = 
    if allIndexVars
    then
      map(
        \ b::Boolean -> 
           if b
           then [top]
           else []
        ,
        top.isAvail
      )
    else
      map(
        \ s::String -> [top]
        ,
        top.accessOrder
      );
  
  top.isAvail =
    if allIndexVars
    then
      mapTail(
        \ l::[String] 
        -> !containsAny(stringEq, access, l)
        ,
        top.accessOrder
      )
    else
      map(
        \ s::String -> true
        ,
        top.accessOrder
      );
  
  top.orders =
    if allIndexVars
    then
      [access]
    else
      [];
  
  top.tensors = [tensor];
  
  local allErrors :: [Message] = 
    lErrors 
    ++
    if null(lErrors)
    then 
      tErrors
      ++
      if null(tErrors)
      then
        sErrors
      else []
    else []
    ++
    if indexVarErr
    then [err(top.location, "Some dimensions of the tensor were accessed using index variables, others were not. This is not supported.")]
    else [];
  
  forwards to
    mkErrorCheck(
      allErrors,
      fwrd
    );
}

function getAccess
[String] ::= idx::Exprs env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return 
    case idx of
    | nilExpr() -> []
    | consExpr(h, tl) ->
       case h of
       | declRefExpr(name(i)) -> i
       | _ -> "err"
       end
       ::
       getAccess(tl, env)
    end;
}
