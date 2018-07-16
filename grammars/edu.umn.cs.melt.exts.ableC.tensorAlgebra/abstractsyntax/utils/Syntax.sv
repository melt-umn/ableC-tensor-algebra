grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:utils;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production orderof
top::Expr ::= tp::Type
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("orderof("),
      tp.lpp,
      tp.rpp,
      text(")")
    ]);
  
  local format::Name =
    case tp of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;
  
  local lErrors::[Message] =
    case tp of
    | tensorType(_, _, _) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, "orderof expected a tensor type (got ${showType(tp.typerep)})")]
    end;
  
  forwards to 
    mkErrorCheck(
      lErrors, 
      mkIntConst(format.tensorFormat.dimensions, top.location)
    );
}

abstract production dimenof
top::Expr ::= tensor::Expr
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("dimenof("),
      tensor.pp,
      text(")")
    ]);
  
  local lErrors::[Message] =
    case tensor.typerep of
    | tensorType(_, fmt, _) -> fmt.tensorFormatLookupCheck
    | _ -> [err(top.location, "orderof expected a tensor type (got ${showType(tensor.typerep)})")]
    end;
  
  local format::Name =
    case tensor.typerep of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local fmtNm::String = fmt.proceduralName;
  local dimens::String = toString(fmt.dimensions);
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution("__tensor", tensor) :: [],
      parseExpr(s"""
      ({
        struct tensor_${fmtNm}* _tensor = &(__tensor);
        unsigned long* dims = GC_malloc(sizeof(unsigned long) * ${dimens});
        memcpy(dims, _tensor->dims, sizeof(unsigned long) * ${dimens});
        dims;
      })
      """)
    );
  
  forwards to
    mkErrorCheck(
      lErrors,
      fwrd
    );
}