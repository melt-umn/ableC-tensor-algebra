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

nonterminal TensorExpr with
  exprName, tensorName, accesses, tensorExpr, envr, 
  tensors, exprs, accessOrder, remaining, isAvail, 
  variable, fmts, sparse, dense, location;

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
  top.dense = [];
}

abstract production tensorAccess
top::TensorExpr ::= ex::Expr tensor::Expr idx::Expr env::Decorated Env
{
  tensor.env = env;
  tensor.returnType = nothing();

  top.exprName = "__none";
  top.exprs = [];

  local fmt::TensorFormat =
    case tensor.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;

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

  top.dense =
    if !prs.isJust
    then []
    else
      if type == storeDense
      then [pair(top.tensorName, dim)]
      else [];
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
  top.dense = l.dense ++ r.dense;
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
  top.dense = l.dense ++ r.dense;
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
  top.dense = l.dense ++ r.dense;
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
  top.dense = l.dense ++ r.dense;
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
String ::= e::TensorExpr
{
  return
    case e of
    | tensorAdd(_, l, r, _) ->
      "(" ++ exprToString(l) ++ " + " ++ exprToString(r) ++ ")"
    | tensorSub(_, l, r, _) ->
      "(" ++ exprToString(l) ++ " - " ++ exprToString(r) ++ ")"
    | tensorMul(_, l, r, _) ->
      "(" ++ exprToString(l) ++ " * " ++ exprToString(r) ++ ")"
    | tensorDiv(_, l, r, _) ->
      "(" ++ exprToString(l) ++ " / " ++ exprToString(r) ++ ")"
    | tensorBaseExpr(ex, _) -> 
      show(100, ex.pp)
    | tensorAccess(_, t, i, _) ->
      show(100, t.pp) ++ "[" ++ show(100, i.pp) ++ "]"
    end;
}
