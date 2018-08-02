grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production accessTensor
top::Expr ::= tensor::Expr idx::Expr env::Decorated Env
{
  propagate substituted;

  top.tensorExp =
    tensorAccess(top, tensor, idx, env, location=top.location);

  local fmt::TensorFormat =
    case tensor.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
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
      getTypereps(idx, env)
    );

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
  
  local fmtNm::String = fmt.proceduralName;
  
  top.pp = ppConcat([
             tensor.pp,
             text("("),
             idx.pp,
             text(")")
           ]);

  local fwrd::Expr =
    if anyIndexVars
    then
      emptyAccess
    else
      substExpr(
        declRefSubstitution("__tensor", tensor)
        :: generateExprsSubs(idx, 0, env),
        parseExpr(
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
            unsigned long __index[] = { ${generateExprsArray(idx, 0, env)} };
            tensor_pack_${fmtNm}(_tensor);
            tensor_get_${fmtNm}(_tensor, __index);
          })
          """
        )
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
    else [];
  
  forwards to
    mkErrorCheck(
      allErrors,
      fwrd
    );
}

function getTypereps
[Type] ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(l, r) ->
       l.typerep :: getTypereps(r, env)
    | _ -> idx.typerep :: []
    end;
}

function getCount
Integer ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(_, r) ->
       1 + getCount(r, env)
    | _ -> 1
    end;
}
