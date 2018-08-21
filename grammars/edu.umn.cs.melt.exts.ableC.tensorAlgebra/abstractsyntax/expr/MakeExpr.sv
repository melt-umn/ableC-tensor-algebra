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
TensorFormat ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  return
    case e of
    | tensorAccess(_, ex, _, _) ->
      case decorate ex with {env=e.envr; returnType=nothing();}.typerep of
      | tensorType(_, f, _) -> new(f.tensorFormat)
      | _ -> 
        case tm:lookup(e.tensorName, fmts) of
        | [] -> errorTensorFormat()
        | x::_ -> x
        end
      end
    | _ -> errorTensorFormat()
    end;
}

function getExprName
String ::= e::Expr env::Decorated Env
{
  return
    case decorate e with {env=env; returnType=nothing();} of
    | declRefExpr(nm) -> nm.name
    | _ -> s"_expr_${toString(e.location.line)}_${toString(e.location.column)}"
    end;
}
