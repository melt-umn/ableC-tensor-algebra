grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function generateExprsSubs
[Substitution] ::= ex::Expr int::Integer env::Decorated Env
{
  ex.env = env;
  ex.returnType = nothing();

  return
    case ex of
    | commaExpr(l, r) ->
       declRefSubstitution(s"__expr_${toString(int)}", l)
       :: generateExprsSubs(r, int+1, env)
    | e -> declRefSubstitution(s"__expr_${toString(int)}", e)
       :: []
    end;
}

function generateExprsArray
String ::= ex::Expr int::Integer env::Decorated Env
{
  ex.env = env;
  ex.returnType = nothing();

  return
    case ex of
    | commaExpr(l, r) ->
       s"__expr_${toString(int)}, " ++ generateExprsArray(r, int+1, env)
    | _ -> s"__expr_${toString(int)}"
    end;
}
