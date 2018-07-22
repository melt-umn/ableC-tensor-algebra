grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function getTensorName
String ::= e::TensorExpr
{
  return
    case e of
    | tensorAccess(_, ex, _, _) -> 
      case decorate ex with {env=e.envr; returnType=nothing();} of
      | declRefExpr(name(s)) -> s
      | _ -> s"_tensor_${toString(e.location.line)}_${toString(e.location.column)}"
      end
    | _ -> s"__error"
    end;
}

function getTensorFormat
TensorFormat ::= e::TensorExpr
{
  return
    case e of
    | tensorAccess(_, ex, _, _) ->
      case decorate ex with {env=e.envr; returnType=nothing();}.typerep of
      | tensorType(_, f, _) -> new(f.tensorFormat)
      | _ -> errorTensorFormat()
      end
    | _ -> errorTensorFormat()
    end;
}
