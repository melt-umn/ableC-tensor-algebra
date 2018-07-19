grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

synthesized attribute condition :: String;

nonterminal TensorCond with condition;

abstract production allCond
top::TensorCond ::=
{
  top.condition = "1";
}

abstract production nullCond
top::TensorCond ::=
{
  top.condition = "0";
}

abstract production sparseAccess
top::TensorCond ::= tensor::Expr dim::Integer env::Decorated Env
{
  tensor.returnType = nothing();
  tensor.env = env;

  local tNm::String = tensor.tensorName;
  top.condition = s"(p${tNm}${toString(dim+1)} < ${tNm}${toString(dim+1)}_pos[p${tNm}${toString(dim)} + 1])";
}

abstract production denseAccess
top::TensorCond ::= tensor::Expr var::String dim::Integer env::Decorated Env
{
  tensor.returnType = nothing();
  tensor.env = env;

  local tNm::String = tensor.tensorName;
  top.condition = s"(${var} < ${tNm}${toString(dim+1)}_size)";
}

abstract production andCond
top::TensorCond ::= l::TensorCond r::TensorCond
{
  top.condition = s"(${l.condition} && ${r.condition})";
}

function condAnd
TensorCond ::= l::TensorCond r::TensorCond
{
  return
    case l of
    | allCond() -> r
    | nullCond() -> r
    | _ -> 
      case r of
      | allCond() -> l
      | nullCond() -> l
      | _ -> andCond(l, r)
      end
    end;
}
