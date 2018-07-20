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
top::TensorCond ::= tNm::String dim::Integer
{
  top.condition = s"(p${tNm}${toString(dim+1)} < ${tNm}${toString(dim+1)}_pos[${if dim == 0 then "1" else s"p${tNm}${toString(dim)} + 1"}])";
}

abstract production denseAccess
top::TensorCond ::= tNm::String dim::Integer var::String
{
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
