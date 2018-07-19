grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production assignTensor
top::Expr ::= l::Expr r::Expr env::Decorated Env
{
  l.lValue = true;
  r.lValue = false;
  
  propagate substituted;
  top.pp = ppConcat([
             l.pp,
             text(" = "),
             r.pp
           ]);

  local lAcc::[String] =
    nubBy(stringEq, flatMap(\l::[String] -> l, l.orders));
  
  local rAcc::[String] =
    nubBy(stringEq, flatMap(\l::[String] -> l, r.orders));
  
  local onlyL::[String] =
    filter(
      \ s::String
      -> !containsBy(stringEq, s, rAcc)
      ,
      lAcc
    );

  local access::[String] =
    merge_access(l.orders ++ r.orders);
  
  local lErrors::[Message] =
    if !null(onlyL)
    then [err(top.location, s"Cannot generate code for this calculation since the index variable${if listLength(onlyL) > 1 then "s" else ""} ${implode(", ", onlyL)} occur${if listLength(onlyL) > 1 then "" else "s"} only on the left-hand side.")]
    else
      if null(access) 
      then [err(top.location, "Cannot generate code for this calculation due to cyclical access pattern.")]
      else [];
  
  r.output = l;
  l.output = l;
  
  r.accessOrder = access;
  l.accessOrder = access;
  
  l.tensorNames = 
    generateTensorNames(l.tensors, "l", 0, env);
  
  r.tensorNames = 
    generateTensorNames(r.tensors, "r", 0, env);
  
  l.subNames =
    generateSubNames(l.canSub, access);
  
  r.subNames = 
    generateSubNames(l.canSub, access);

  local tensorSubs::[Pair<String Expr>] =
    findTensorSubs(l.tensors, "l", 0, env, true)
    ++
    findTensorSubs(r.tensors, "r", 0, env, false);
  
  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors ++ lErrors,
      mkStringConst(s"${implode(", ", map((.condition), r.conds))}", top.location)
    );
}

function merge_access
[String] ::= acc::[[String]]
{
  local lowers::[String] =
    flatMap(
      \ lst::[String]
      -> case lst of
         | [] -> []
         | _ :: [] -> []
         | _ :: xs -> xs
         end
      ,
      acc         
    );
  
  local topVars::[String] =
    nubBy(
      stringEq,
      map(
        head(_)
        ,
        filter(\lst::[String] -> !null(lst), acc)
      )
    );
  
  local notUsed::[Boolean] =
    map(
      \ v::String
      -> !containsBy(stringEq, v, lowers)
      ,
      topVars
    );
  
  local found::Boolean =
    foldl( \b1::Boolean b2::Boolean -> b1 || b2, false, notUsed);
  
  local v::String =
    head(
      map(
        \l::Pair<String Boolean> -> l.fst,
        filter(
          \ p::Pair<String Boolean>
          -> p.snd
          ,
          zipWith(pair, topVars, notUsed)
        )
      )
    );
  
  local res::[[String]] =
    filter(
      \ lst::[String] -> !null(lst),
      map(
        \ lst::[String]
        -> filter(
             \ st::String -> st != v
             ,
             lst
           )
        ,
        acc
      )
    );
  
  return
    if !found
    then []
    else 
      if null(res)
      then v :: []
      else 
        let en::[String] = merge_access(res)
        in
          if null(en)
          then []
          else v :: en
        end;
}

function generateTensorNames
[String] ::= tensors::[Expr] pfx::String idx::Integer env::Decorated Env
{
  return
    if null(tensors)
    then []
    else
      case decorate head(tensors) with {env=env; returnType=nothing();} of
      | declRefExpr(name(nm)) -> 
         nm 
         :: 
         generateTensorNames(tail(tensors), pfx, idx, env)
      | _ -> 
         s"__tensor_${pfx}_${toString(idx)}"
         ::
         generateTensorNames(tail(tensors), pfx, idx+1, env)
      end;
}

function findTensorSubs
[Pair<String Expr>] ::= tensors::[Expr] pfx::String idx::Integer env::Decorated Env lValue::Boolean
{
  return
    if null(tensors)
    then []
    else 
      case decorate head(tensors) with {env=env; returnType=nothing(); lValue=lValue;} of
      | declRefExpr(name(_)) ->
         findTensorSubs(tail(tensors), pfx, idx, env, lValue)
      | _ ->
         pair(s"__tensor_${pfx}_${toString(idx)}", head(tensors))
         ::
         findTensorSubs(tail(tensors), pfx, idx+1, env, lValue)
      end;
}

function generateSubNames
[[String]] ::= sbs::[[Expr]] vars::[String]
{
  return
    if null(sbs)
    then []
    else
      let v::String = head(vars)
      in
      map(
        \ i::Integer
        -> s"t${v}${toString(i)}"
        ,
        makeList(integerCompare, inc, 0, listLength(head(sbs)))
      )
      ::
      generateSubNames(tail(sbs), tail(vars))
      end; 
}
