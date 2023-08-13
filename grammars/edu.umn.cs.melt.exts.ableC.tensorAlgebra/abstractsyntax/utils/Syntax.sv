grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:utils;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- The orderof production when a type is used -}
abstract production orderofType
top::Expr ::= tp::TypeName
{
  top.pp = 
    ppConcat([
      text("orderof("),
      tp.pp,
      text(")")
    ]);

  propagate controlStmtContext, env;

  forwards to orderof(tp.typerep);
}

{- The orderof production when an Expr is used -}
abstract production orderofExpr
top::Expr ::= e::Expr
{
  top.pp = 
    ppConcat([
      text("orderof("),
      e.pp,
      text(")")
    ]);

  propagate controlStmtContext, env;

  local fwrd::Expr = orderof(e.typerep);
  forwards to 
    mkErrorCheck(
      e.errors,
      fwrd
    );
}

{- The orderof production, which returns the order of a
   tensor -}
abstract production orderof
top::Expr ::= tp::Type
{
  top.pp =
    ppConcat([
      text("orderof("),
      tp.lpp,
      tp.rpp,
      text(")")
    ]);
  
  local format::Name =
    case tp of
    | extType(_, tensorType(fmt)) -> fmt
    | _ -> name("__error__")
    end;
  format.env = top.env;
  
  local lErrors::[Message] =
    checkTensorHeader(top.env) ++
    case tp of
    | extType(_, tensorType(_)) -> format.tensorFormatLookupCheck
    | _ -> [errFromOrigin(top, s"orderof expected a tensor type (got ${showType(tp)})")]
    end;
  
  local fwrd::Expr = mkIntConst(format.tensorFormat.dimensions);
  forwards to 
    mkErrorCheck(
      lErrors, 
      fwrd
    );
}

{- The production for determining the size of a specific
   dimension of a tensor. This can only be used in the 
   form dimenof(Expr)[Expr], so the dimension must be
   directly accessed, preventing the user access to the
   dimension array -}
abstract production dimenof
top::Expr ::= tensor::Expr dim::Expr
{
  top.pp =
    ppConcat([
      text("dimenof("),
      tensor.pp,
      text(")"),
      text("["),
      dim.pp,
      text("]")
    ]);

  propagate controlStmtContext, env;

  local lErrors::[Message] =
    checkTensorHeader(top.env)
    ++
    case tensor.typerep of
    | extType(_, tensorType(fmt)) -> fmt.tensorFormatLookupCheck
    | _ -> [errFromOrigin(top, s"dimenof expected a tensor type (got ${showType(tensor.typerep)})")]
    end
    ++
    tensor.errors
    ++
    dim.errors
    ++
    if dim.typerep.isIntegerType
    then
      if dim.integerConstantValue.isJust
      then -- If dim is a constant, we error check it now
        let c::Integer = dim.integerConstantValue.fromJust
        in
        if c < 0 || c >= fmt.dimensions
        then [errFromOrigin(top, s"attempting to access an invalid dimension of the tensor with dimenof (requested ${toString(c)}, have ${toString(fmt.dimensions)})")]
        else []
        end
      else []
    else [errFromOrigin(top, s"dimenof expected an integer for dimension (got ${showType(dim.typerep)})")]; 
  
  local format::Name =
    case tensor.typerep of
    | extType(_, tensorType(fmt)) -> fmt
    | _ -> name("__error__")
    end;
  format.env = top.env;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    if dim.integerConstantValue.isJust
    then -- If dim is a constant, no runtime error checking needed
      ableC_Expr {
        ((struct $name{s"tensor_${fmtNm}"}) $Expr{tensor}).dims[$Expr{dim}]
      }
    else -- Otherwise, we error check at runtime
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = (struct $name{s"tensor_${fmtNm}"}*) &$Expr{tensor};
          unsigned long dim = $Expr{dim};
          if(dim >= $intLiteralExpr{fmt.dimensions}) {
            fprintf(stderr, 
              $stringLiteralExpr{s"Attempted to access dimenof at dimension %lu, tensor only has %d dimensions. (At ${getParsedOriginLocationOrFallback(top).unparse})\n"}, 
              dim, 
              $intLiteralExpr{fmt.dimensions});
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
