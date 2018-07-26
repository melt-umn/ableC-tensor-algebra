grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

imports silver:langutil:pp;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute exprName :: String;
synthesized attribute tensorName :: String;
synthesized attribute accesses :: [[String]];
synthesized attribute tensorExpr :: Expr;
synthesized attribute tensors :: [TensorExpr];
synthesized attribute exprs :: [Expr];
synthesized attribute envr :: Decorated Env;

autocopy attribute accessOrder :: [String];

autocopy attribute remaining :: [String];
synthesized attribute isAvail :: Boolean;

autocopy attribute variable :: String;
synthesized attribute sparse :: [Pair<String Integer>];
synthesized attribute dense :: [Pair<String Integer>];
synthesized attribute sparse_r :: [Pair<String Integer>];
synthesized attribute dense_r :: [Pair<String Integer>];
synthesized attribute next_sparse :: Maybe<Pair<String Integer>>;

nonterminal TensorExpr with
  exprName, tensorName, accesses, tensorExpr, envr, 
  tensors, exprs, accessOrder, remaining, isAvail, 
  variable, fmts, sparse, dense, sparse_r, dense_r,
  next_sparse, location;

abstract production tensorBaseExpr
top::TensorExpr ::= ex::Expr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = [];
  
  top.exprName = getExprName(ex, env);
  top.exprs = [ex];

  top.tensorExpr = ex;
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
top::TensorExpr ::= ex::Expr tensor::Expr idx::Expr env::Decorated Env
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

  top.tensorExpr = ex;
  top.envr = env;
  
  top.accesses = [access];

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
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.tensorExpr = ex;
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
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.tensorExpr = ex;
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
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.tensorExpr = ex;
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
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.exprName = "__none";
  top.exprs = l.exprs ++ r.exprs;

  top.tensorExpr = ex;
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

function exprToString
String ::= e::TensorExpr fmts::tm:Map<String TensorFormat>
{
  e.fmts = fmts;

  return
    case e of
    | tensorAdd(_, l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "+" ++ exprToString(r, fmts) ++ ")"
    | tensorSub(_, l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "-" ++ exprToString(r, fmts) ++ ")"
    | tensorMul(_, l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "*" ++ exprToString(r, fmts) ++ ")"
    | tensorDiv(_, l, r, _) ->
      "(" ++ exprToString(l, fmts) ++ "/" ++ exprToString(r, fmts) ++ ")"
    | tensorBaseExpr(ex, _) -> 
      show(100, ex.pp)
    | tensorAccess(_, t, i, _) ->
      e.tensorName ++ "[" ++ implode(",", head(e.accesses)) ++ "]"
    end;
}

function modifyNames
TensorExpr ::= names::[String] ex::TensorExpr
{
  return
    case ex of
    | tensorBaseExpr(_, _) -> ex
    | tensorAccess(e, t, i, n) ->
      if ex.tensorName != head(names)
      then 
        tensorAccess(
          e,
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
    | tensorAdd(e, l, r, n) ->
      tensorAdd(
        e,
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
    | tensorSub(e, l, r, n) ->
      tensorSub(
        e,
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
    | tensorMul(e, l, r, n) ->
      tensorMul(
        e,
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
    | tensorDiv(e, l, r, n) ->
      tensorDiv(
        e,
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
