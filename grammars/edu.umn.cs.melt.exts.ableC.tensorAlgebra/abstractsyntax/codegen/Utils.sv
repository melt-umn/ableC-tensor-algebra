grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declAll
Decl ::= fmt::TensorFormatItem
{
  return decls(
    consDecl(
      declStruct(fmt),
      consDecl(
        declInsertFunction(fmt),
        consDecl(
          declTensorGet(fmt),
          consDecl(
            declMakeFunction(fmt),
            consDecl(
              declPackFunction(fmt),
              consDecl(
                declMakeFilledFunction(fmt),
                consDecl(
                  declModifyFunction(fmt),
                  nilDecl()
                )
              )
            )
          )
        )
      )
    ));
}

function getElem
Maybe<a> ::= lst::[a] idx::Integer
{
  return
    if null(lst)
    then nothing()
    else if idx <= 0
    then just(head(lst))
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

function generateIndexArray
String ::= size::Integer
{
  return if size == 1
         then s"i1"
         else s"${generateIndexArray(size-1)}, i${toString(size)}";
}

function orderList
[a] ::= lst::[a] order::[Integer]
{
  return
    case order of
    | [] -> []
    | h::tl -> case getElem(lst, h) of
               | nothing() -> []
               | just(i) -> [i]
               end
               ++ orderList(lst, tl)
    end;
}

function findIndexOf
Integer ::= eq::(Boolean ::= a a) lst::[a] elem::a idx::Integer
{
  return case lst of
         | [] -> -1
         | h::tl ->
             if eq(h, elem) 
             then idx
             else findIndexOf(eq, tl, elem, idx+1)
         end;
}
