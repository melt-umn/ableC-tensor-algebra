grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

function generateExprsSubs
[Substitution] ::= ex::Exprs int::Integer
{
  return
    case ex of
    | consExpr(e, tl) ->
       declRefSubstitution(s"__expr_${toString(int)}", e)
       :: generateExprsSubs(tl, int+1)
    | nilExpr() -> []
    end;
}

function generateExprsArray
String ::= ex::Exprs int::Integer
{
  return
    case ex of
    | consExpr(_, tl) ->
       case tl of
       | consExpr(_, _) -> s"__expr_${toString(int)}, " ++ generateExprsArray(tl, int+1)
       | nilExpr() -> s"__expr_${toString(int)}"
       end
    | nilExpr() -> ""
    end;
}
