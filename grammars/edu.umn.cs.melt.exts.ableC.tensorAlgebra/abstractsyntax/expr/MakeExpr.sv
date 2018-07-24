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

function generateTensorVals
String ::= ex::TensorExpr fmts::tm:Map<String TensorFormat>
{
  local fmt::TensorFormat =
    getTensorFormat(ex, fmts);
  local nm::String =
    getTensorName(ex);

  return
    s"double* ${nm}_data = ${nm}.data;"
    ++
    "\n"
    ++
    implode("\n",
      map(
        \ p::Pair<Integer Pair<Integer Integer>> ->
          if p.snd.snd == storeDense
          then 
            s"unsigned long ${nm}${toString(p.fst+1)}_size = ${nm}.indices[${toString(p.snd.fst)}][0][0];"
          else
            s"unsigned long* ${nm}${toString(p.fst+1)}_pos = ${nm}.indices[${toString(p.snd.fst)}][0];"
            ++
            "\n"
            ++
            s"unsigned long* ${nm}${toString(p.fst+1)}_idx = ${nm}.indices[${toString(p.snd.fst)}][1];"
        ,
        fmt.storage
      )
    );
}
