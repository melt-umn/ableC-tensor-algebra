grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production accessTensorAssign
top::Expr ::= tensor::Expr idx::Expr op::(Expr ::= Expr Expr Location) right::Expr env::Decorated Env
{
  propagate substituted;

  local rightTensorExpr :: Boolean =
    case moduleName(env, right.typerep) of
    | nothing() -> false
    | just(s) -> s == "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
    end;

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
      getTypereps(idx, env)
    );

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
      getTypereps(idx, env)
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
      getTypereps(idx, env)
    )
    ++
    case tensor.typerep of
    | tensorType(_, f, _) -> f.tensorFormatLookupCheck
    | x -> [err(tensor.location, s"Expected a tensor type, got ${showType(x)}")]
    end;

  local sErrors::[Message] =
    if getCount(idx, env) != fmt.dimensions
    then [err(tensor.location, s"Number of dimensions specified does not match, expected ${toString(fmt.dimensions)}, got ${toString(getCount(idx, env))}.")]
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
             text("["),
             idx.pp,
             text("]")
           ]);

  local fwrd::Expr =
    if rightTensorExpr
    then
      if allIndexVars
      then -- x[i] = A[i,j]
        tensorAssignToTensor(tensor, idx, right, location=top.location) -- perhaps we should add op to this
      else -- x = A[i,j]
        errorExpr([err(top.location, "This should not occur")], location=top.location)
    else -- x[i] = a
      op(
        substExpr(
          declRefSubstitution("__tensor", tensor)
          :: generateExprsSubs(idx, 0, env),
          parseExpr(
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
                unsigned long __index[] = { ${generateExprsArray(idx, 0, env)} };
                tensor_getPointer_${fmtNm}(_tensor, __index);
              })
            """
        )
      )
      ,
      right,
      top.location
    );

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
