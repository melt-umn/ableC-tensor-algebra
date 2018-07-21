grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

imports silver:langutil:pp;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensorName :: String;
synthesized attribute accesses :: [[String]];
synthesized attribute tensorExpr :: Expr;
synthesized attribute tensors :: [TensorExpr];
synthesized attribute envr :: Decorated Env;

autocopy attribute tensorNames :: [String];
autocopy attribute accessOrder :: [String];

nonterminal TensorExpr with
  tensorName, accesses, tensorExpr, envr, tensors, 
  tensorNames, accessOrder, location;

abstract production tensorBaseExpr
top::TensorExpr ::= ex::Expr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = [];
  
  top.tensorExpr = ex;
  top.envr = env;
  
  top.tensors = [];
}

abstract production tensorAccess
top::TensorExpr ::= ex::Expr tensor::Expr idx::Expr env::Decorated Env
{
  tensor.env = env;
  tensor.returnType = nothing();

  local fmt::TensorFormat =
    case tensor.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    | _ -> errorTensorFormat()
    end;

  top.tensorName = head(top.tensorNames);

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
}

abstract production tensorAdd
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.tensorExpr = ex;
  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);
}

abstract production tensorSub
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.tensorExpr = ex;
  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);
}

abstract production tensorMul
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.tensorExpr = ex;
  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);
}

abstract production tensorDiv
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";
  top.accesses = l.accesses ++ r.accesses;

  top.tensorExpr = ex;
  top.envr = env;

  top.tensors = l.tensors ++ r.tensors;

  l.tensorNames = 
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);
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
