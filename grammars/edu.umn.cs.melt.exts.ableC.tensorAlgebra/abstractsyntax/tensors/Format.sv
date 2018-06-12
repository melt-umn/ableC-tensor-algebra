grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:tensors;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute types::[Integer];
synthesized attribute order::[Integer];
synthesized attribute dimens::Integer;
synthesized attribute proceduralName::String;

nonterminal TensorFormat with types, order, dimens, proceduralName;

abstract production tensorFormat
top::TensorFormat ::= types::[Integer] order::[Integer]
{
  top.types = types;
  top.order = order;
  top.proceduralName = formName(types, order);
  top.dimens = arrayLength(types);
}

function formName
String ::= types::[Integer] order::[Integer]
{
  return case types, order of
         | [], [] -> ""
         | s::ss, o::os ->
           (if s == storeDense
            then "d"
            else "s")
           ++ toString(o) ++ formName(ss, os)
         | _, _ -> "error"
         end;
}
