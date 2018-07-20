grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function getTensorName
String ::= e::TensorExpr
{
  return
    case e of
    | tensorBaseExpr(ex, _) -> 
      case decorate ex with {env=e.envr; returnType=nothing();} of
      | declRefExpr(name(s)) -> s
      | _ -> s"_tensor_${toString(e.location.line)}_${toString(e.location.column)}"
      end
    | _ -> s"__error"
    end;
}
