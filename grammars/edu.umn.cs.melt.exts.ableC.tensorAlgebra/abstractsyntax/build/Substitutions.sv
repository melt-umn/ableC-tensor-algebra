grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

function generateSubstitutions
[Substitution] ::= lst::[Expr] index::Integer
{
  return
    if !null(lst)
    then
      declRefSubstitution(s"__arr${toString(index)}__", head(lst)) 
      ::
      generateSubstitutions(tail(lst), index+1)
    else [];
}

function generateArray
String ::= lst::[Expr] index::Integer
{
  return
    case lst of
    | [] -> ""
    | _::[] -> s"__arr${toString(index)}__"
    | _::tl -> s"__arr${toString(index)}__, ${generateArray(tl, index+1)}"
    end;
}
