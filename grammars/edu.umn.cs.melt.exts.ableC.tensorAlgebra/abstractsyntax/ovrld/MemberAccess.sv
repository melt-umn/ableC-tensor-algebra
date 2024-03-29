grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production accessMember
top::Expr ::= tensor::Expr deref::Boolean nm::Name
{
  top.pp = 
    if deref
    then pp"${tensor.pp}->${text(nm.name)}"
    else pp"${tensor.pp}.${text(nm.name)}";

  propagate controlStmtContext, env;

  local fmt::TensorFormat =
    case tensor.typerep of
    | extType(_, tensorType(f)) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;

  local lErrors :: [Message] =
    case nm.name of
    | "dataLen" -> []
    | _ -> [errFromOrigin(top, s"Tensors do not have a '${nm.name}' member")]
    end
    ++
    case tensor.typerep of
    | extType(_, tensorType(_)) -> []
    | _ -> [errFromOrigin(top, "Invalid Tensor Member Access")]
    end;

  local fwrd::Expr = ableC_Expr {
      ((struct $name{s"tensor_${fmt.proceduralName}"}) $Expr{tensor}).$name{nm.name}
    };
  forwards to
    mkErrorCheck(lErrors, fwrd);
}
