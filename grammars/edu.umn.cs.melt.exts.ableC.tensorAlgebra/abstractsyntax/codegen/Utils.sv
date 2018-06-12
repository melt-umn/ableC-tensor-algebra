grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declAll
Decl ::= fmt::TensorFormat
{
  return decls(
    consDecl(
      declMakeFilledFunction(fmt),
      consDecl(
        declModifyFunction(fmt),
        nilDecl()
      )
    )
  );
}

function getElem
a ::= lst::[a] idx::Integer
{
  return
    if idx <= 0
    then head(lst)
    else getElem(tail(lst), idx - 1);
}

function generateProductDims
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if dims == idx
    then s"1"
    else s"dims[${index}] * ${generateProductDims(dims, idx + 1)}";
}
