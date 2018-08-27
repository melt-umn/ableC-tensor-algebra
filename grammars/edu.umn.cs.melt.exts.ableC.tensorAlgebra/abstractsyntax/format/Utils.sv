grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Produce the procedural name of a tensor format, given the list of
   specifiers (sparse or dense) and the order of the dimensions. -}
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

{- Build the 'storage' attribute of the TensorFormat nonterminal -}
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

{- Test if a tensor format only has dense dimensions. Used in Halide and
   a few other places to remove unecessary code. -}
function allDense
Boolean ::= fmt::TensorFormat
{
  return
    foldl(
      \ b::Boolean p::Pair<Integer Pair<Integer Integer>> ->
        b && p.snd.snd == storeDense
      ,
      true,
      fmt.storage
    );
}
