grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production accessTensor
top::Expr ::= tensor::Expr idx::Exprs
{
  propagate substituted;
  
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
    | tensorType(_, _, _) -> []
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
  
  local fmt::TensorFormat = new(format.tensorFormat);
  local fmtNm::String = fmt.proceduralName;
  
  top.pp = ppConcat([
             tensor.pp,
             text("("),
             ppImplode(text(", "), idx.pps),
             text(")")
           ]);
  
  local fwrd::Expr =
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
  
  forwards to
    mkErrorCheck(
      if null(lErrors)
      then
        if null(tErrors)
        then sErrors
        else tErrors
      else lErrors
      ,
      fwrd
    );
}
