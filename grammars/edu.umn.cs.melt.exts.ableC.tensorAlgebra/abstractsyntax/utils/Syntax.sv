grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:utils;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production orderofType
top::Expr ::= tp::TypeName
{
  propagate substituted;
  top.pp = 
    ppConcat([
      text("orderof("),
      tp.pp,
      text(")")
    ]);

  forwards to orderof(tp.typerep, location=top.location);
}

abstract production orderofExpr
top::Expr ::= e::Expr
{
  propagate substituted;
  top.pp = 
    ppConcat([
      text("orderof("),
      e.pp,
      text(")")
    ]);

  forwards to orderof(e.typerep, location=top.location);
}

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
    checkTensorHeader(top.location, top.env)
    ++
    case tp of
    | tensorType(_, _, _) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"orderof expected a tensor type (got ${showType(tp)})")]
    end;
  
  forwards to 
    mkErrorCheck(
      lErrors, 
      mkIntConst(format.tensorFormat.dimensions, top.location)
    );
}

abstract production dimenof
top::Expr ::= tensor::Expr dim::Expr
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("dimenof("),
      tensor.pp,
      text(")"),
      text("["),
      dim.pp,
      text("]")
    ]);

  local lErrors::[Message] =
    checkTensorHeader(top.location, top.env)
    ++
    case tensor.typerep of
    | tensorType(_, fmt, _) -> fmt.tensorFormatLookupCheck
    | _ -> [err(top.location, s"dimenof expected a tensor type (got ${showType(tensor.typerep)})")]
    end
    ++
    tensor.errors
    ++
    dim.errors
    ++
    if dim.typerep.isIntegerType
    then
      if dim.integerConstantValue.isJust
      then
        let c::Integer = dim.integerConstantValue.fromJust
        in
        if c < 0 || c >= fmt.dimensions
        then [err(top.location, s"attempting to access an invalid dimension of the tensor with dimenof (requested ${toString(c)}, have ${toString(fmt.dimensions)})")]
        else []
        end
      else []
    else [err(top.location, s"dimenof expected an integer for dimension (got ${showType(dim.typerep)})")]; 
  
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
    if dim.integerConstantValue.isJust
    then
      ableC_Expr {
        $Expr{tensor}.dims[$Expr{dim}]
      }
    else
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = &$Expr{tensor};
          unsigned long dim = $Expr{dim};
          if(dim >= $intLiteralExpr{fmt.dimensions}) {
            fprintf(stderr, "Attempted to access dimenof at dimension %lu, tensor only has %lu dimensions.\n", dim, $intLiteralExpr{fmt.dimensions});
            exit(1);
          }
          _tensor->dims[dim];
        })
      };
  
  forwards to
    mkErrorCheck(
      lErrors,
      fwrd
    );
}
