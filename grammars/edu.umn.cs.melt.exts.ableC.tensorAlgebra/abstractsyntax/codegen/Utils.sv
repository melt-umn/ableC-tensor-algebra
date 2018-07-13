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

function accessTensor
Expr ::= tensor::Expr idx::Exprs loc::Location env::Decorated Env
{
  tensor.env = env;
  tensor.returnType = nothing();
  idx.env = env;
  idx.returnType = nothing();

  local lErrors::[Message] = tensor.errors ++ idx.errors;
  
  local tErrors::[Message] = 
    flatMap(
      \ t::Type
      -> t.errors
         ++
         if listLength(t.errors) == 0 || t.isIntegerType
         then []
         else [err(tensor.location, s"Expected integer type, got ${showType(t)}")]
      ,
      idx.typereps
    )
    ++
    case tensor.typerep of
    | pointerType(_, tensorType(_, _, _)) -> []
    | x -> [err(tensor.location, s"Expected a tensor type, got ${showType(x)}")]
    end;
  
  local sErrors::[Message] =
    if idx.count != fmt.dimens
    then [err(tensor.location, s"Number of dimensions specified does not match, expected ${toString(fmt.dimens)}, got ${toString(idx.count)}.}")]
    else [];
    
  local format::Name =
    case tensor.typerep of
    | pointerType(_, tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=tensor.location)
    end;
  format.env = env;
  
  local fmt::TensorFormatItem = new(format.tensorFormatItem);
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__tensor", tensor)
      :: generateExprsSubs(idx, 0),
      parseExpr(
        if tensor.isLValue
        then "*"
        else ""
        ++
        s"""
        ({
          struct tensor_${fmtNm}* _tensor = __tensor;
          unsigned long __index[] = { ${generateExprsArray(idx, 0)} };
          ${if tensor.isLValue
            then s"""
              tensor_access_${fmtNm}(_tensor, __index);
            """
            else s"""
              tensor_pack_${fmtNm}(_tensor);
              tensor_get_${fmtNm}(_tensor, __index);
            """
          }
        })
        """
      )
    );

  return mkErrorCheck(
           if null(lErrors) 
           then if null(tErrors)
                then sErrors
                else tErrors
           else lErrors
           , 
           fwrd);
}

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
        | consExpr(_, _) -> "__expr_${toString(int)}, " ++ generateExprsArray(tl, int+1)
        | nilExpr() -> "__expr_${toString(int)}"
        end
    | nilExpr() -> ""
    end;
}
