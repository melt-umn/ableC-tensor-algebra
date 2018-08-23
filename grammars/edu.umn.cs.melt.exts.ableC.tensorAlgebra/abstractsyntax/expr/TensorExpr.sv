grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- A nonterminal representing an expression that can be used in a 
   tensor expression.
-}

-- If this TensorExpr is just an Expr, a procedural name 
-- based on location (prevents multiple calls to functions)
synthesized attribute exprName :: String;

-- The name of the tensor if this is a tensor access
synthesized attribute tensorName :: String;

-- The variables and needed order of all tensor accesses 
-- in this TensorExpr
synthesized attribute accesses :: [[String]];

synthesized attribute tensors :: [TensorExpr];
synthesized attribute exprs :: [Expr];
synthesized attribute envr :: Decorated Env;

-- The order of variables in the current computation
autocopy attribute accessOrder :: [String];

-- Used by foreach to determine how a dimension is accessed
synthesized attribute iterAccess :: [Either<Expr String>];

-- Setting remaining and querying isAvail tells whether
-- at the current position in the codegen a TensorExpr
-- is available to calculate.
autocopy attribute remaining :: [String];
synthesized attribute isAvail :: Boolean;

-- The variable that is currently used (being queried of)
autocopy attribute variable :: String;

-- Various ways to determine if layers are dense or sparse.
-- Lists are mapped over the accessOrder.
synthesized attribute sparse :: [Pair<String Integer>];
synthesized attribute dense :: [Pair<String Integer>];
synthesized attribute sparse_r :: [Pair<String Integer>];
synthesized attribute dense_r :: [Pair<String Integer>];
synthesized attribute next_sparse :: Maybe<Pair<String Integer>>;

nonterminal TensorExpr with
  exprName, tensorName, accesses, envr, 
  tensors, exprs, accessOrder, remaining, isAvail, 
  variable, fmts, sparse, dense, sparse_r, dense_r,
  iterAccess, next_sparse, location;

abstract production tensorBaseExpr
top::TensorExpr ::= ex::Expr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = [];
  top.iterAccess = [];
  
  top.exprName = getExprName(ex, env);
  top.exprs = [ex];

  top.envr = env;
  
  top.tensors = [];

  top.isAvail = true;

  top.sparse = [];
  top.sparse_r = [];
  top.dense = [];
  top.dense_r = [];
  top.next_sparse = nothing();
}

abstract production tensorAccess
top::TensorExpr ::= tensor::Expr idx::Expr env::Decorated Env
{
  tensor.env = env;
  tensor.returnType = nothing();

  top.exprName = "__none";
  top.exprs = [];

  local fmt::TensorFormat =
    getTensorFormat(top, top.fmts);

  top.tensorName = getTensorName(top);

  local access::[String] =
    orderList(
      getAccess(idx, env),
      map(
        \ p::Pair<Integer Pair<Integer Integer>>
        -> p.snd.fst
        ,
        fmt.storage
      )
    );

  local iterAcc :: [Either<Expr String>] =
    orderList(
      getIterAccess(idx, env),
      map(
        \ p::Pair<Integer Pair<Integer Integer>>
        -> p.snd.fst
        ,
        fmt.storage
      )
    );

  top.envr = env;
  
  top.accesses = [access];
  top.iterAccess = iterAcc;

  top.tensors = [top];

  top.isAvail =
    !containsAny(
      stringEq,
      top.remaining,
      access
    );

  local f::TensorFormat =
    head(tm:lookup(top.tensorName, top.fmts));

  local dim::Integer =
    positionOf(
      stringEq,
      top.variable,
      access
    );

  local prs::Maybe<Pair<Integer Pair<Integer Integer>>> =
    getElem(f.storage, dim);

  local type::Integer =
    prs.fromJust.snd.snd;

  top.sparse =
    if !prs.isJust
    then []
    else
      if type == storeSparse
      then [pair(top.tensorName, dim)]
      else [];
  
  top.sparse_r =
    if !prs.isJust
    then []
    else
      if type == storeSparse
      then [pair(top.tensorName, prs.fromJust.snd.fst)]
      else [];

  top.dense =
    if !prs.isJust
    then []
    else
      if type == storeDense
      then [pair(top.tensorName, dim)]
      else [];

  top.dense_r =
    if !prs.isJust
    then []
    else
      if type == storeDense
      then [pair(top.tensorName, prs.fromJust.snd.fst)]
      else [];

  top.next_sparse =
    let dNext::Maybe<Pair<Integer Pair<Integer Integer>>> =
      getElem(f.storage, dim+1)
    in
    if !dNext.isJust
    then nothing()
    else
      if dNext.fromJust.snd.snd == storeSparse
      then just(pair(top.tensorName, dim+1))
      else nothing()
    end;
}

abstract production tensorAdd
top::TensorExpr ::= l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;
  top.iterAccess = [];

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  top.isAvail = l.isAvail && r.isAvail;

  top.sparse = l.sparse ++ r.sparse;
  top.sparse_r = l.sparse_r ++ r.sparse_r;
  top.dense = l.dense ++ r.dense;
  top.dense_r = l.dense_r ++ r.dense_r;
  top.next_sparse = nothing();
}

abstract production tensorSub
top::TensorExpr ::= l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;
  top.iterAccess = [];

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  top.isAvail = l.isAvail && r.isAvail;

  top.sparse = l.sparse ++ r.sparse;
  top.sparse_r = l.sparse_r ++ r.sparse_r;
  top.dense = l.dense ++ r.dense;
  top.dense_r = l.dense_r ++ r.dense_r;
  top.next_sparse = nothing();
}

abstract production tensorMul
top::TensorExpr ::= l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;
  top.iterAccess = [];

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  top.isAvail = l.isAvail && r.isAvail;

  top.sparse = l.sparse ++ r.sparse;
  top.sparse_r = l.sparse_r ++ r.sparse_r;
  top.dense = l.dense ++ r.dense;
  top.dense_r = l.dense_r ++ r.dense_r;
  top.next_sparse = nothing();
}

abstract production tensorDiv
top::TensorExpr ::= l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;
  top.iterAccess = [];

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  top.isAvail = l.isAvail && r.isAvail;

  top.sparse = l.sparse ++ r.sparse;
  top.sparse_r = l.sparse_r ++ r.sparse_r;
  top.dense = l.dense ++ r.dense;
  top.dense_r = l.dense_r ++ r.dense_r;
  top.next_sparse = nothing();
}

abstract production nullTensorExpr
top::TensorExpr ::= env::Decorated Env
{
  forwards to
    tensorBaseExpr(
      mkIntConst(0, top.location),
      env,
      location=top.location
    );
}

{- Takes idx, a comma-seperated list of the index
   variables used to access a tensor, and returns
   the indexvar names
-}
function getAccess
[String] ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(l, r) ->
       case l of
       | declRefExpr(name(i)) ->
          i :: getAccess(r, env)
       | _ -> "__error" :: []
       end
    | declRefExpr(name(i)) -> i :: []
    | _ -> "__error" :: []
    end;
}

{- Similar to getAccess, but works with foreach when
   may be indexvar or an Expr
-}
function getIterAccess
[Either<Expr String>] ::= idx::Expr env::Decorated Env
{
  idx.env = env;
  idx.returnType = nothing();

  return
    case idx of
    | commaExpr(l, r) ->
      case l.typerep of
      | indexVarType(_) ->
        case l of
        | declRefExpr(name(i)) -> 
          right(i) :: getIterAccess(r, env)
        | _ -> 
          left(l) :: getIterAccess(r, env)
        end
      | _ -> 
        left(l) :: getIterAccess(r, env)
      end
    | _ -> 
      case idx.typerep of
      | indexVarType(_) -> 
        case idx of
        | declRefExpr(name(i)) ->
          right(i) :: []
        | _ -> 
          left(idx) :: []
        end
      | _ ->
        left(idx) :: []
      end
    end;
}

function exprToString
String ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorAdd(l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "+" ++ exprToString(r, fmts) ++ ")"
    | tensorSub(l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "-" ++ exprToString(r, fmts) ++ ")"
    | tensorMul(l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "*" ++ exprToString(r, fmts) ++ ")"
    | tensorDiv(l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "/" ++ exprToString(r, fmts) ++ ")"
    | tensorBaseExpr(ex, _) -> 
      show(100, ex.pp)
    | tensorAccess(t, i, _) ->
      e.tensorName ++ "[" ++ implode(",", head(e.accesses)) ++ "]"
    end;
}

{- Changes names of tensors in a TensorExpr based on the names
   passed in. This is used for resolving equations like 
   a[i] = b[i] * b[i]
-}
function modifyNames
TensorExpr ::= names::[String] ex::TensorExpr
{
  return
    case ex of
    | tensorBaseExpr(_, _) -> ex
    | tensorAccess(t, i, n) ->
      if ex.tensorName != head(names)
      then 
        tensorAccess(
          declRefExpr(
            name(
              head(names),
              location=ex.location
            ),
            location=ex.location
          ),
          i,
          n,
          location=ex.location
        )
      else ex
    | tensorAdd(l, r, n) ->
      tensorAdd(
        modifyNames(
          take(
            listLength(l.tensors),
            names
          ),
          l
        ),
        modifyNames(
          drop(
            listLength(l.tensors),
            names
          ),
          r
        ),
        n,
        location=ex.location
      )
    | tensorSub(l, r, n) ->
      tensorSub(
        modifyNames(
          take(
            listLength(l.tensors),
            names
          ),
          l
        ),
        modifyNames(
          drop(
            listLength(l.tensors),
            names
          ),
          r
        ),
        n,
        location=ex.location
      )
    | tensorMul(l, r, n) ->
      tensorMul(
        modifyNames(
          take(
            listLength(l.tensors),
            names
          ),
          l
        ),
        modifyNames(
          drop(
            listLength(l.tensors),
            names
          ),
          r
        ),
        n,
        location=ex.location
      )
    | tensorDiv(l, r, n) ->
      tensorDiv(
        modifyNames(
          take(
            listLength(l.tensors),
            names
          ),
          l
        ),
        modifyNames(
          drop(
            listLength(l.tensors),
            names
          ),
          r
        ),
        n,
        location=ex.location
      )
    end;
}
