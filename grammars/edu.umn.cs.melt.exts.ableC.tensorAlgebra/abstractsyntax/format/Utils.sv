grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function formName
String ::= specs::[Integer] order::[Integer]
{
  return 
    case specs, order of
    | [], [] -> ""
    | s::ss, o::os ->
      (if s == storeDense
       then "d"
       else "s")
      ++
      toString(o)
      ++
      formName(ss, os)
    | _, _ -> "error"
    end;
}

function formStorage
[Pair<Integer Pair<Integer Integer>>] ::= specs::[Integer] order::[Integer] idx::Integer
{
  return
    case order of
    | [] -> []
    | o::os ->
       case getElem(specs, o) of
       | just(x) -> pair(idx, pair(o, x)) :: formStorage(specs, os, idx+1)
       | _ -> []
       end
    end;
}
