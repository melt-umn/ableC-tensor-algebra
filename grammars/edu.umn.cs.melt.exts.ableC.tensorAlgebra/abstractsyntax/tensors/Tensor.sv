grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:tensors;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports silver:langutil;
imports silver:langutil:pp;

synthesized attribute dimensions::Integer;
synthesized attribute size::Integer;
synthesized attribute data::Either<[Tensor] [Expr]>;
synthesized attribute err::[Message];
synthesized attribute asArray::String;
synthesized attribute dimArray::String;
synthesized attribute substs::[Substitution];
inherited attribute pos::String;
nonterminal Tensor with location, pp, err, env, dimensions, size, data, asArray, dimArray, substs, pos;

abstract production tensor_higher
t::Tensor ::= sub::[Tensor]
{
  t.pp = pp"[${ppTensorArray(sub)}]";
  t.err = case sub of
          | [] -> [err(t.location, s"Tensor cannot be produced from no tensors, cannot have dimension of 0")]
          | _ -> []
          end ++
          if !checkDimensions(sub)
          then [err(t.location, s"Tensor cannot be produced from tensors of different dimensionality")]
          else []
          ++
          if !checkSizes(sub)
          then [err(t.location, s"Tensor cannot be ragged, sizes of all sub tensors must be the same")]
          else []
          ++ combineErrors(head(sub), tail(sub), t.env);

  t.dimensions = head(sub).dimensions + 1;
  t.size = listLength(sub);

  t.asArray = asArrayTensors(t.pos, head(sub), tail(sub), 0);
  t.dimArray = toString(t.size) ++ ", " ++ head(sub).dimArray;
  t.substs = combineSubsts(t.pos, head(sub), tail(sub), 0);

  t.data = left(sub);
}

function asArrayTensors
String ::= pos::String h::Tensor tl::[Tensor] idx::Integer
{
  h.pos = pos ++ "_" ++ toString(idx);

  return if null(tl)
         then h.asArray
         else h.asArray ++ ", " ++ asArrayTensors(pos, head(tl), tail(tl), idx+1);
}

function combineSubsts
[Substitution] ::= pos::String h::Tensor tl::[Tensor] idx::Integer
{
  h.pos = pos ++ "_" ++ toString(idx);
  
  return if null(tl)
         then h.substs
         else h.substs ++ combineSubsts(pos, head(tl), tail(tl), idx+1);
}

function combineErrors
[Message] ::= h::Tensor tl::[Tensor] env::Decorated Env
{
  h.env = env;
  
  return if !null(tl)
         then h.err ++ combineErrors(head(tl), tail(tl), env)
         else h.err;
}

abstract production tensor_base
t::Tensor ::= sub::[Expr]
{
  t.pp = pp"[${ppExprArray(sub)}]";
  t.err = case sub of
          | [] -> [err(t.location, s"Tensor cannot be produced from no values, cannot have dimension of 0")]
          | _ -> []
          end
          ++ checkTypes(head(sub), tail(sub), t.env)
          ++ errorChecking(head(sub), tail(sub), t.env);

  t.dimensions = 1;
  t.size = listLength(sub);

  t.asArray = asArrayExprs(t.pos, sub, 0);
  t.dimArray = toString(t.size);
  t.substs = generateSubstsExprs(t.pos, sub, 0);

  t.data = right(sub);
}

function errorChecking
[Message] ::= h::Expr tl::[Expr] env::Decorated Env
{
  h.env = env;

  h.returnType = nothing();
  return case h of
         | errorExpr(e) -> e
         | _ -> []
         end ++
         if null(tl)
         then []
         else errorChecking(head(tl), tail(tl), env);
}

function asArrayExprs
String ::= pos::String sub::[Expr] idx::Integer
{
  return if null(tail(sub))
         then s"__tensor_data_${pos}_${toString(idx)}"
         else s"""
           __tensor_data_${pos}_${toString(idx)}, ${asArrayExprs(pos, tail(sub), idx+1)}
         """;
}

function generateSubstsExprs
[Substitution] ::= pos::String sub::[Expr] idx::Integer
{
  return if !null(sub)
         then declRefSubstitution(s"__tensor_data_${pos}_${toString(idx)}",
                                  head(sub)) ::
              generateSubstsExprs(pos, tail(sub), idx+1)
         else [];
}

function checkTypes
[Message] ::= h::Expr lst::[Expr] env::Decorated Env
{
  h.env = env;
  
  h.returnType = nothing();
  return if !h.typerep.isArithmeticType
         then [err(h.location, s"Tensor data must be have numeric type (got ${showType(h.typerep)})")]
         else []
         ++ if !null(lst)
         then checkTypes(head(lst), tail(lst), env)
         else [];
}

function checkDimensions
Boolean ::= lst::[Tensor]
{
  return dimensionsCheck(tail(lst), head(lst).dimensions);
}

function dimensionsCheck
Boolean ::= lst::[Tensor] dim::Integer
{
  return if !null(lst)
         then if head(lst).dimensions != dim
              then false
              else dimensionsCheck(tail(lst), dim)
         else true;
}

function checkSizes
Boolean ::= lst::[Tensor]
{
  return sizesCheck(tail(lst), head(lst).size);
}

function sizesCheck
Boolean ::= lst::[Tensor] size::Integer
{
  return if !null(lst)
         then if head(lst).size != size
              then false
              else sizesCheck(tail(lst), size)
         else true;
}

function ppTensorArray
Document ::= lst::[Tensor]
{
  return case lst of
         | [] -> pp""
         | x::[] -> x.pp
         | x::xs -> pp"${x.pp}, ${ppTensorArray(xs)}"
         end;
}

function ppExprArray
Document ::= lst::[Expr]
{
  return case lst of
         | [] -> pp""
         | x::[] -> x.pp
         | x::xs -> pp"${x.pp}, ${ppExprArray(xs)}"
         end;
}
