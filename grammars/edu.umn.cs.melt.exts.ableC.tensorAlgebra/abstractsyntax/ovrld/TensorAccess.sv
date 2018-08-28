grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Read a value out of a tensor (or access a tensor using
   indexvars) -}
abstract production accessTensor
top::Expr ::= tensor::Expr idx::Expr env::Decorated Env
{
  propagate substituted;

  top.tensorExp =
    tensorAccess(tensor, idx, env, location=top.location);

  local fmt::TensorFormat =
    case tensor.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;


  {- Check if idx is an array, in which case we handle
     the forward separately -}
  local arrayAccess :: Boolean =
    case idx.typerep of
    | pointerType(_, _) -> true
    | arrayType(_, _, _, _) -> true
    | _ -> false
    end;

  {- If idx is an array, the type of the values in it -}
  local arrType :: Type =
    case idx.typerep of
    | pointerType(_, t) -> t
    | arrayType(e, _, _, _) -> e
    | _ -> idx.typerep
    end;

  {- Check if any value in the index is an indexvar -}
  local anyIndexVars::Boolean =
    foldl(
      \ b::Boolean t::Type
      -> b ||
         case t of
         | indexVarType(_) -> true
         | _ -> false
         end
      ,
      false,
      getTypereps(idx, env) -- Function to parse commaExpr
    );

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
           if listLength(t.errors) != 0 || t.isIntegerType
           then []
           else [err(idx.location, s"Expected integer type, got ${showType(t)}")]
        ,
        getTypereps(idx, env)
      )
    )
    ++
    case tensor.typerep of
    | tensorType(_, f, _) -> f.tensorFormatLookupCheck
    | x -> [err(tensor.location, s"Expected a tensor type, got ${showType(x)}")]
    end;
  
  local sErrors::[Message] =
    if !arrayAccess && getCount(idx, env) != fmt.dimensions
    then [err(tensor.location, s"Number of dimensions specified does not match, expected ${toString(fmt.dimensions)}, got ${toString(getCount(idx, env))}.")]
    else [];
  
  local fmtNm::String = fmt.proceduralName;
  
  top.pp = ppConcat([
             tensor.pp,
             text("("),
             idx.pp,
             text(")")
           ]);

  local idxInitializer :: Initializer =
    objectInitializer(
      generateInitList(idx, env)
    );

  local fwrd::Expr =
    if arrayAccess
    then
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = &$Expr{tensor};
          $BaseTypeExpr{idx.typerep.baseTypeExpr}* __idx = $Expr{idx};
          unsigned long _idx[$intLiteralExpr{fmt.dimensions}];
          
          for(unsigned long __d = 0; __d < $intLiteralExpr{fmt.dimensions}; __d++) {
            _idx[__d] = __idx[__d]; // Copy to get type right
          }
          $name{s"tensor_pack_${fmtNm}"}(_tensor);
          __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
          double res = $name{s"tensor_get_${fmtNm}"}(_tensor, _idx);
          res;
        })
      }
    else
    if anyIndexVars
    then
      emptyAccess -- a tensor_acc
    else
      ableC_Expr {
        ({
          struct $name{s"tensor_${fmtNm}"}* _tensor = &$Expr{tensor};
          unsigned long __index[$intLiteralExpr{fmt.dimensions}] = $Initializer{idxInitializer};
          $name{s"tensor_pack_${fmtNm}"}(_tensor);
          __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
          $name{s"tensor_get_${fmtNm}"}(_tensor, __index);
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
    else [];
  
  forwards to
    mkErrorCheck(
      allErrors,
      fwrd
    );
}

-- Function to parse an index, which uses 
-- commaExpr, into an InitList which can be used
-- to initialize an array.
function generateInitList
InitList ::= ex::Expr env::Decorated Env
{
  ex.env = env;
  ex.returnType = nothing();

  return
    case ex of
    | commaExpr(l, r) ->
      consInit(
        positionalInit(
          exprInitializer(l)
        ),
        generateInitList(r, env)
      )
    | _ ->
      consInit(
        positionalInit(
          exprInitializer(ex)
        ),
        nilInit()
      )
    end;
}

-- Function to get the types of all elements in
-- the index
function getTypereps
[Type] ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(l, r) ->
       l.typerep :: getTypereps(r, env)
    | _ -> idx.typerep :: []
    end;
}

-- Count the number of elements in the index
function getCount
Integer ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(_, r) ->
       1 + getCount(r, env)
    | _ -> 1
    end;
}
