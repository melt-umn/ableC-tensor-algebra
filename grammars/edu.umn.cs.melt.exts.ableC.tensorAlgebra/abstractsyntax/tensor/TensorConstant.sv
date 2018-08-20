grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:tensor;

synthesized attribute tensor_dims::Integer;
synthesized attribute tensor_size::Integer;
synthesized attribute tensor_data::Either<[TensorConstant] [Expr]>;

synthesized attribute tensor_asExpr :: Initializer;
synthesized attribute tensor_dimExpr :: Initializer;

nonterminal TensorConstant with location, pp, errors, env, tensor_dims, tensor_size, tensor_data, tensor_asExpr, tensor_dimExpr;

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
  
  t.tensor_asExpr = 
    objectInitializer(
      let lsts :: [InitList] =
        map(
          \ c::TensorConstant ->
            case 
              decorate 
                (decorate c with {env=t.env;}).tensor_asExpr 
              with 
              {env=t.env; returnType=nothing();} 
            of
            | objectInitializer(l) -> l
            end
          ,
          sub
        )
      in
      concatInitList(lsts)
      end
    );
  t.tensor_dimExpr =
    objectInitializer(
      consInit(
        positionalInit(
          exprInitializer(
            mkIntConst(t.tensor_size, t.location)
          )
        ),
        case 
          decorate 
            (decorate head(sub) with {env=t.env;}).tensor_dimExpr 
          with 
          {env=t.env; returnType=nothing();} 
        of
        | objectInitializer(l) -> l
        end
      )
    );

  t.tensor_data = left(sub);
}

function concatInitList
InitList ::= lst::[InitList]
{
  return
    case lst of
    | [] -> nilInit()
    | i::[] -> i
    | h::tl -> appendInitList(h, concatInitList(tl))
    end;
}

function appendInitList
InitList ::= a::InitList b::InitList
{
  return
    case a of
    | nilInit() -> b
    | consInit(i, tl) ->
      consInit(i, appendInitList(tl, b))
    end;
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
  
  t.tensor_asExpr =
    objectInitializer(
      foldr(
        \ sb::Expr lst::InitList ->
          consInit(
            positionalInit(
              exprInitializer(sb)
            ),
            lst
          )
        ,
        nilInit(),
        sub
      )
    );
  t.tensor_dimExpr =
    objectInitializer(
      consInit(
        positionalInit(
          exprInitializer(
            mkIntConst(t.tensor_size, t.location)
          )
        ),
        nilInit()
      )
    );

  t.tensor_data = right(sub);
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
    if null(tl)
    then []
    else errorChecking(head(tl), tail(tl), env);
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
