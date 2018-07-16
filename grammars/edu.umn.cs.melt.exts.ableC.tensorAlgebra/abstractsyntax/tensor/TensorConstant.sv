grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:tensor;

synthesized attribute tensor_dims::Integer;
synthesized attribute tensor_size::Integer;
synthesized attribute tensor_data::Either<[TensorConstant] [Expr]>;
synthesized attribute tensor_asArray::String;
synthesized attribute tensor_dimArray::String;
synthesized attribute tensor_substs::[Substitution];
inherited attribute tensor_pos::String;

nonterminal TensorConstant with location, pp, errors, env, tensor_dims, tensor_size, tensor_data, tensor_asArray, tensor_dimArray, tensor_substs, tensor_pos;

abstract production tensor_higher
t::TensorConstant ::= sub::[TensorConstant]
{
  t.pp = ppConcat([
           text("["),
           ppImplode(text(", "),
             map(
               \ t::TensorConstant
               -> t.pp
               ,
               sub
             )
           ),
           text("]")
         ]);

  t.errors := 
    case sub of
    | [] -> [err(t.location, "Tensor cannot be produced from no tensors, cannot hae dimension of 0")]
    | _ -> []
    end
    ++
    (
    if !checkDimensions(sub)
    then [err(t.location, "Tensor cannot be produced from tensors of different dimensionality")]
    else []
    )
    ++
    (
    if !checkSizes(sub)
    then [err(t.location, "Tensor cannot be ragged, sizes of all sub tensors must be the same")]
    else []
    )
    ++
    combineErrors(
      head(sub),
      tail(sub),
      t.env
    );
  
  t.tensor_dims = head(sub).tensor_dims + 1;
  t.tensor_size = listLength(sub);
  
  t.tensor_asArray = asArrayTensors(t.tensor_pos, head(sub), tail(sub), 0);
  t.tensor_dimArray = toString(t.tensor_size) ++ ", " ++ head(sub).tensor_dimArray;
  t.tensor_substs = combineSubsts(t.tensor_pos, head(sub), tail(sub), 0);
  
  t.tensor_data = left(sub);
}

abstract production tensor_base
t::TensorConstant ::= sub::[Expr]
{
  t.pp = ppConcat([
           text("["),
           ppImplode(
             text(", "),
             map(
               \ e::Expr -> e.pp,
               sub
             )
           ),
           text("]")
         ]);
  t.errors := 
    case sub of
    | [] -> [err(t.location, s"Tensor cannot be produced from no values, cannot have dimension of 0")]
    | _ -> []
    end
    ++
    errorChecking(head(sub), tail(sub), t.env);
  
  t.tensor_dims = 1;
  t.tensor_size = listLength(sub);
  
  t.tensor_asArray = asArrayExprs(t.tensor_pos, sub, 0);
  t.tensor_dimArray = toString(t.tensor_size);
  t.tensor_substs = generateSubstsExprs(t.tensor_pos, sub, 0);
  
  t.tensor_data = right(sub);
}

function asArrayExprs
String ::= pos::String sub::[Expr] idx::Integer
{
  return
    if null(tail(sub))
    then s"__tensor_data_${pos}_${toString(idx)}"
    else s"""
      __tensor_data_${pos}_${toString(idx)}, ${asArrayExprs(pos, tail(sub), idx+1)}
    """;
}

function asArrayTensors
String ::= pos::String h::TensorConstant tl::[TensorConstant] idx::Integer
{
  h.tensor_pos = pos ++ "_" ++ toString(idx);
  
  return
    if null(tl)
    then h.tensor_asArray
    else h.tensor_asArray ++ ", " ++ asArrayTensors(pos, head(tl), tail(tl), idx+1);
}

function combineSubsts
[Substitution] ::= pos::String h::TensorConstant tl::[TensorConstant] idx::Integer
{
  h.tensor_pos = pos ++ "_" ++ toString(idx);
  
  return
    if null(tl)
    then h.tensor_substs
    else h.tensor_substs ++ combineSubsts(pos, head(tl), tail(tl), idx+1);
}

function combineErrors
[Message] ::= h::TensorConstant tl::[TensorConstant] env::Decorated Env
{
  h.env = env;

  return if !null(tl)
         then h.errors ++ combineErrors(head(tl), tail(tl), env)
         else h.errors;
}

function errorChecking
[Message] ::= h::Expr tl::[Expr] env::Decorated Env
{
  h.env = env;
  
  h.returnType = nothing();
  return
    (
    if null(h.errors)
    then 
      if h.typerep.isArithmeticType
      then []
      else [err(h.location, "Expected an arithmetic value, got ${showType(h.typerep)}.")]
    else h.errors
    )
    ++
    errorChecking(head(tl), tail(tl), env);
}

function generateSubstsExprs
[Substitution] ::= pos::String sub::[Expr] idx::Integer
{
  return
    if !null(sub)
    then 
      declRefSubstitution(
        s"__tensor_data_${pos}_${toString(idx)}",
        head(sub)
      )
      ::
      generateSubstsExprs(pos, tail(sub), idx+1)
    else [];
}

function checkDimensions
Boolean ::= lst::[TensorConstant]
{
  return dimensionsCheck(tail(lst), head(lst).tensor_dims);
}

function dimensionsCheck
Boolean ::= lst::[TensorConstant] dim::Integer
{
  return
    if !null(lst)
    then 
      if head(lst).tensor_dims != dim
      then false
      else dimensionsCheck(tail(lst), dim)
    else true;
}

function checkSizes
Boolean ::= lst::[TensorConstant]
{
  return sizesCheck(tail(lst), head(lst).tensor_size);
}

function sizesCheck
Boolean ::= lst::[TensorConstant] size::Integer
{
  return
    if !null(lst)
    then
      if head(lst).tensor_size != size
      then false
      else sizesCheck(tail(lst), size)
    else true;
}
