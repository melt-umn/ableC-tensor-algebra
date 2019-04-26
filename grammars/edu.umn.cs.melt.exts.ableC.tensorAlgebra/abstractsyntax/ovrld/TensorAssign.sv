grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Production to assign a value to an element in a tensor. This
   also handles the needed forwarding to the code generation 
   system. This system uses op as long as it is not a tensor 
   expression. If it is a tensor expression, then the op is 
   ignored. -}
abstract production accessTensorAssign
top::Expr ::= tensor::Expr idx::Expr right::Expr
{

  -- Whether the right side of the expression is a tensor_acc or
  -- An actual value
  local rightTensorExpr :: Boolean =
    case right.typerep of
    | extType(_, tensorAccType()) -> true
    | _ -> false
    end;

  local fmt::TensorFormat =
    case tensor.typerep of
    | extType(_, tensorType(f)) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;

  local arrayAccess :: Boolean =
    case idx.typerep of
    | pointerType(_, _) -> true
    | arrayType(_, _, _, _) -> true
    | _ -> false
    end;

  local arrType :: Type =
    case idx.typerep of
    | pointerType(_, t) -> t
    | arrayType(e, _, _, _) -> e
    | _ -> idx.typerep
    end;

  local access::[String] =
    orderList(
      getAccess(idx, top.env),
      map(
        \ p::Pair<Integer Pair<Integer Integer>>
        -> p.snd.fst
        ,
        fmt.storage
      )
    );

  local types::[Integer] =
    map(
      \ p::Pair<Integer Pair<Integer Integer>> -> p.snd.snd
      ,
      fmt.storage
    );

  local allIndexVars::Boolean =
    foldl(
      \ b::Boolean t::Type
      -> b &&
         case t of
         | extType(_, indexvarType()) -> true
         | _ -> false
         end
      ,
      true,
      getTypereps(idx, top.env)
    );

  local anyIndexVars::Boolean =
    foldl(
      \ b::Boolean t::Type
      -> b ||
         case t of
         | extType(_, indexvarType()) -> true
         | _ -> false
         end
      ,
      false,
      getTypereps(idx, top.env)
    );

  local indexVarErr::Boolean =
    anyIndexVars && !allIndexVars;

  local lErrors::[Message] = tensor.errors ++ idx.errors;

  local tErrors::[Message] =
    (
    if arrayAccess
    then
      if arrType.isIntegerType
      then []
      else [err(idx.location, s"Expected an integer array, instead got ${showType(arrType)} array.")]
    else
      flatMap(
        \ t::Type
        -> t.errors
           ++
           if listLength(t.errors) != 0 || t.isIntegerType ||
             case t of extType(_, indexvarType()) -> true | _ -> false end
           then []
           else [err(tensor.location, s"Expected integer type, got ${showType(t)}")]
        ,
        getTypereps(idx, top.env)
      )
    )
    ++
    case tensor.typerep of
    | extType(_, tensorType(f)) -> f.tensorFormatLookupCheck
    | x -> [err(tensor.location, s"Expected a tensor type, got ${showType(x)}")]
    end
    ++
    if !rightTensorExpr && allIndexVars
    then [err(tensor.location, s"Cannot have indexvars on only the left-hand side.")]
    else [];

  local sErrors::[Message] =
    if !arrayAccess && getCount(idx, top.env) != fmt.dimensions
    then [err(tensor.location, s"Number of dimensions specified does not match, expected ${toString(fmt.dimensions)}, got ${toString(getCount(idx, top.env))}.")]
    else [];

  local format::Name =
    case tensor.typerep of
    | extType(_, tensorType(fmt)) -> fmt
    | _ -> name("__error__", location=tensor.location)
    end;
  format.env = top.env;

  local fmtNm::String = fmt.proceduralName;

  top.pp = ppConcat([
             tensor.pp,
             text("["),
             idx.pp,
             text("]")
           ]);

  local idxInit :: Initializer =
    objectInitializer(
      generateInitList(idx, top.env)
    );

  local fwrd::Expr =
    if arrayAccess
    then
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = (struct $name{s"tensor_${fmtNm}"}*) &$Expr{tensor};
          $BaseTypeExpr{idx.typerep.baseTypeExpr}* __idx = $Expr{idx};
          unsigned long _idx[$intLiteralExpr{fmt.dimensions}];
          
          for(unsigned long __d = 0; __d < $intLiteralExpr{fmt.dimensions}; __d++) {
            _idx[__d] = __idx[__d];
          }
          pthread_rwlock_wrlock(&(_tensor->lock));
          __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
          double* res = $name{s"tensor_getPointer_locked_${fmtNm}"}(_tensor, _idx);
          *res = $Expr{right};
          pthread_rwlock_unlock(&(_tensor->lock));
          *res;
        })
      }
    else
    if rightTensorExpr
    then
      if allIndexVars
      then -- x[i] = A[i,j]
        tensorAssignToTensor(tensor, idx, right, location=top.location) -- perhaps we should add op to this
      else -- x = A[i,j] (handled by seperate overload)
        errorExpr([err(top.location, "This should not occur")], location=top.location)
    else -- x[i] = a
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = (struct $name{s"tensor_${fmtNm}"}*) &$Expr{tensor};
          unsigned long __index[$intLiteralExpr{fmt.dimensions}] = $Initializer{idxInit};
          pthread_rwlock_wrlock(&(_tensor->lock));
          __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
          double* res = $name{s"tensor_getPointer_locked_${fmtNm}"}(_tensor, __index);
          *res = $Expr{right};
          pthread_rwlock_unlock(&(_tensor->lock));
          *res;
        })
      };

  local allErrors :: [Message] =
    lErrors
    ++
    if null(lErrors)
    then 
      tErrors
      ++
      if null(tErrors)
      then
        sErrors
      else []
    else []
    ++
    if indexVarErr
    then [err(top.location, "Some dimensions of the tensor were accessed using index variables, others were not. This is not supported.")]
    else [];

  forwards to
    mkErrorCheck(
      allErrors,
      fwrd
    );
}
