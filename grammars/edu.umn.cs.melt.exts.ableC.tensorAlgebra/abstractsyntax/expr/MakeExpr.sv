grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Helper functions for working with TensorExpr -}

function getTensorName
String ::= e::TensorExpr
{
  local loc::Location = getParsedOriginLocation(e).fromJust;
  return
    case e of
    | tensorAccess(ex, _, _) -> 
      case decorate ex with {env=e.envr;
                  controlStmtContext=initialControlStmtContext;} of
      | decExpr(declRefExpr(name(s))) -> s
      | declRefExpr(name(s)) -> s
      | _ -> s"_tensor_${toString(loc.line)}_${toString(loc.column)}"
      end
    | _ -> s"__error"
    end;
}

function getTensorFormat
TensorFormat ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  return
    case e of
    | tensorAccess(ex, _, _) ->
      case decorate ex with {env=e.envr;
                  controlStmtContext=initialControlStmtContext;}.typerep of
      | extType(_, tensorType(f)) -> new(f.tensorFormat)
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
  local loc::Location = getParsedOriginLocation(e).fromJust;
  return
    case decorate e with {env=env;
                  controlStmtContext=initialControlStmtContext;} of
    | declRefExpr(nm) -> nm.name
    | _ -> s"_expr_${toString(loc.line)}_${toString(loc.column)}"
    end;
}
