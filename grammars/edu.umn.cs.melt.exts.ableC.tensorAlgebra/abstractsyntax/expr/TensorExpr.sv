grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

imports silver:langutil:pp;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensorName :: String;
synthesized attribute tensorExpr :: Expr;
synthesized attribute conds :: [TensorCond];
synthesized attribute subExpr :: [[Expr]];
synthesized attribute sparse :: [[Pair<String Integer>]];
synthesized attribute dense :: [[Pair<String Integer>]];
synthesized attribute canSub :: [[TensorExpr]];
synthesized attribute isAvail :: [Boolean];
synthesized attribute orders :: [[String]];
synthesized attribute tensors :: [TensorExpr];
synthesized attribute subed :: [TensorExpr];
synthesized attribute compute :: String;
synthesized attribute envr :: Decorated Env;

autocopy attribute accessOrder :: [String];
autocopy attribute subNames :: [[String]];
autocopy attribute tensorNames :: [String];
autocopy attribute output :: TensorExpr;

nonterminal TensorExpr with
  tensorName, tensorExpr, conds, subExpr,
  sparse, dense, canSub, isAvail, orders,
  tensors, subed, compute, envr, accessOrder, 
  subNames, tensorNames, output, location;

abstract production tensorBaseExpr
top::TensorExpr ::= ex::Expr env::Decorated Env
{
  top.tensorName = "__none";
  
  top.tensorExpr = ex;
  top.envr = env;
  
  top.conds = 
    map(
      \ s::String -> allCond()
      ,
      top.accessOrder
    );
  
  top.subExpr =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );

  top.sparse =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );

  top.dense =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );

  top.canSub =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );

  top.isAvail = 
    map(
      \ s::String -> true
      ,
      top.accessOrder
    );

  top.orders = [];

  top.tensors = [];

  top.subed =
    map(
      \ s::String -> top
      ,
      top.accessOrder
    );

  top.compute = "exit(1);";
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

  local types::[Integer] =
    map(
      \ p::Pair<Integer Pair<Integer Integer>> -> p.snd.snd
      ,
      fmt.storage
    );

  top.tensorName = head(top.tensorNames);

  top.tensorExpr = ex;
  top.envr = env;

  top.conds =
    map(
      \ s::String ->
        let i::Integer =
          positionBy(
            \ is::String -> is == s
            ,
            access
          )
        in
        if i == -1
        then nullCond()
        else
          if case getElem(types, i) of
             | nothing() -> false
             | just(x) -> x == storeSparse
             end
          then sparseAccess(top.tensorName, i)
          else denseAccess(top.tensorName, i, s)
        end
      ,
      top.accessOrder
    );

  top.subExpr =
    map(
      \ s::String -> []
      ,
      top.accessOrder
    );

  top.sparse =
    map(
      \ s::String ->
         let i::Integer =
           positionBy(
             \ is::String -> is == s
             ,
             access
           )
         in
         if i == -1
         then []
         else 
           if case getElem(types, i) of
              | nothing() -> true
              | just(x) -> x == storeDense
              end
           then []
           else [pair(top.tensorName, i)]
         end
      ,
      top.accessOrder
    );

  top.dense =
    map(
      \ s::String ->
         let i::Integer =
           positionBy(
             \ is::String -> is == s
             ,
             access
           )
         in
         if i == -1
         then []
         else
           if case getElem(types, i) of
              | nothing() -> true
              | just(x) -> x == storeSparse
              end
           then []
           else [pair(top.tensorName, i)]
         end
      ,
      top.accessOrder
    );

  top.canSub =
    zipWith(
      \ this::Boolean prev::Boolean
      -> if this && !prev
         then [top]
         else []
      ,
      top.isAvail,
      false :: top.isAvail
    );

  top.isAvail =
    mapTail(
      \ l::[String] ->
        !containsAny(stringEq, access, l)
      ,
      top.accessOrder
    );

  top.orders = [access];

  top.tensors = [top];
  
  top.subed =
    zip3(
      \ sbs::[String] sbd::TensorExpr avail::Boolean ->
        if avail
        then
          if null(sbs)
          then sbd
          else 
            tensorBaseExpr(
              declRefExpr(
                name(head(sbs), location=top.location), 
                location=top.location
              ),
              env,
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );
  
  top.compute = generateCompute(top);
}

abstract production tensorAdd
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__name";

  top.tensorExpr = ex;
  top.envr = env;

  top.conds =
    zipWith(
      \ cl::TensorCond cr::TensorCond ->
        condAnd(cl, cr)
      ,
      l.conds,
      r.conds
    );

  top.subExpr =
    zipWith(
      \ sl::[Expr] sr::[Expr] ->
        l.tensorExpr :: r.tensorExpr :: 
        map(
          \ e::Expr ->
            addTensor(e, r.tensorExpr, location=top.location)
          ,
          sl
        )
        ++
        map(
          \ e::Expr ->
            addTensor(l.tensorExpr, e, location=top.location)
          ,
          sr
        )
      ,
      l.subExpr,
      r.subExpr
    );

  top.sparse =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.sparse,
      r.sparse
    );

  top.dense =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.dense,
      r.dense
    );

  top.canSub =
    zip3(
      \ ls::Pair<[TensorExpr] [TensorExpr]> avail::Boolean availLast::Boolean ->
        if avail && !availLast
        then [top]
        else 
          if avail
          then []
          else ls.fst ++ ls.snd
      ,
      zipWith(pair, l.canSub, r.canSub),
      top.isAvail,
      false :: top.isAvail
    );

  top.isAvail =
    zipWith(
      \ bl::Boolean br::Boolean ->
        bl && br
      ,
      l.isAvail,
      r.isAvail
    );

  top.orders = 
    l.orders ++ r.orders;

  top.tensors =
    l.tensors ++ r.tensors;

  top.subed =
    zip3(
      \ sbs::[String] sbd::TensorExpr avail::Boolean ->
        if avail
        then
          if null(sbs)
          then sbd
          else 
            tensorBaseExpr(
              declRefExpr(
                name(head(sbs), location=top.location),
                location=top.location
              ),
              env,
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );

  l.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail
        then []
        else take(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  r.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail 
        then []
        else drop(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);

  top.compute = generateCompute(top);
}

abstract production tensorSub
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__name";

  top.tensorExpr = ex;
  top.envr = env;

  top.conds =
    zipWith(
      \ cl::TensorCond cr::TensorCond ->
        condAnd(cl, cr)
      ,
      l.conds,
      r.conds
    );

  top.subExpr =
    zipWith(
      \ sl::[Expr] sr::[Expr] ->
        l.tensorExpr :: r.tensorExpr :: 
        map(
          \ e::Expr ->
            subTensor(e, r.tensorExpr, location=top.location)
          ,
          sl
        )
        ++
        map(
          \ e::Expr ->
            subTensor(l.tensorExpr, e, location=top.location)
          ,
          sr
        )
      ,
      l.subExpr,
      r.subExpr
    );

  top.sparse =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.sparse,
      r.sparse
    );

  top.dense =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.dense,
      r.dense
    );

  top.canSub =
    zip3(
      \ ls::Pair<[TensorExpr] [TensorExpr]> avail::Boolean availLast::Boolean ->
        if avail && !availLast
        then [top]
        else 
          if avail
          then []
          else ls.fst ++ ls.snd
      ,
      zipWith(pair, l.canSub, r.canSub),
      top.isAvail,
      false :: top.isAvail
    );

  top.isAvail =
    zipWith(
      \ bl::Boolean br::Boolean ->
        bl && br
      ,
      l.isAvail,
      r.isAvail
    );

  top.orders = 
    l.orders ++ r.orders;

  top.tensors =
    l.tensors ++ r.tensors;

  top.subed =
    zip3(
      \ sbs::[String] sbd::TensorExpr avail::Boolean ->
        if avail
        then
          if null(sbs)
          then sbd
          else 
            tensorBaseExpr(
              declRefExpr(
                name(head(sbs), location=top.location),
                location=top.location
              ),
              env,
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );

  l.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail
        then []
        else take(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  r.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail 
        then []
        else drop(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);

  top.compute = generateCompute(top);
}

abstract production tensorMul
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";

  top.tensorExpr = ex;
  top.envr = env;

  top.conds =
    zipWith(
      \ cl::TensorCond cr::TensorCond ->
        condAnd(cl, cr)
      ,
      l.conds,
      r.conds
    );

  top.subExpr =
    zipWith(
      \ sl::[Expr] sr::[Expr] ->
        map(
          \ e::Expr ->
            mulTensor(e, r.tensorExpr, location=top.location)
          ,
          sl
        )
        ++
        map(
          \ e::Expr ->
            mulTensor(l.tensorExpr, e, location=top.location)
          ,
          sr
        )
      ,
      l.subExpr,
      r.subExpr
    );

  top.sparse =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.sparse,
      r.sparse
    );

  top.dense =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.dense,
      r.dense
    );

  top.canSub =
    zip3(
      \ ls::Pair<[TensorExpr] [TensorExpr]> avail::Boolean availLast::Boolean ->
        if avail && !availLast
        then [top]
        else
          if avail
          then []
          else ls.fst ++ ls.snd
      ,
      zipWith(pair, l.canSub, r.canSub),
      top.isAvail,
      false :: top.isAvail
    );

  top.isAvail =
    zipWith(
      \ bl::Boolean br::Boolean ->
        bl && br
      ,
      l.isAvail,
      r.isAvail
    );

  top.orders = 
    l.orders ++ r.orders;

  top.tensors =
    l.tensors ++ r.tensors;

  top.subed = 
    zip3(
      \ sbs::[String] sbd::TensorExpr avail::Boolean ->
        if avail
        then 
          if null(sbs)
          then sbd
          else 
            tensorBaseExpr(
              declRefExpr(
                name(head(sbs), location=top.location),
                location=top.location
              ),
              env,
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );

  l.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail
        then []
        else take(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  r.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail 
        then []
        else drop(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames = 
    drop(listLength(l.tensors), top.tensorNames);

  top.compute = generateCompute(top);
}

abstract production tensorDiv
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr env::Decorated Env
{
  top.tensorName = "__none";

  top.tensorExpr = ex;
  top.envr = env;

  top.conds =
    zipWith(
      \ cl::TensorCond cr::TensorCond ->
        condAnd(cl, cr)
      ,
      l.conds,
      r.conds
    );

  top.subExpr =
    zipWith(
      \ sl::[Expr] sr::[Expr] ->
        map(
          \ e::Expr ->
            divTensor(e, r.tensorExpr, location=top.location)
          ,
          sl
        )
        ++
        map(
          \ e::Expr ->
            divTensor(l.tensorExpr, e, location=top.location)
          ,
          sr
        )
      ,
      l.subExpr,
      r.subExpr
    );

  top.sparse =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.sparse,
      r.sparse
    );

  top.dense =
    zipWith(
      \ ll::[Pair<String Integer>] lr::[Pair<String Integer>] ->
        nubBy(
          \ p1::Pair<String Integer> p2::Pair<String Integer> ->
            p1.fst == p2.fst
            && p1.snd == p2.snd
          ,
          ll ++ lr
        )
      ,
      l.dense,
      r.dense
    );

  top.canSub =
    zip3(
      \ ls::Pair<[TensorExpr] [TensorExpr]> avail::Boolean availLast::Boolean ->
        if avail && !availLast
        then [top]
        else
          if avail
          then []
          else ls.fst ++ ls.snd
      ,
      zipWith(pair, l.canSub, r.canSub),
      top.isAvail,
      false :: top.isAvail
    );

  top.isAvail =
    zipWith(
      \ bl::Boolean br::Boolean ->
        bl && br
      ,
      l.isAvail,
      r.isAvail
    );

  top.orders = 
    l.orders ++ r.orders;

  top.tensors =
    l.tensors ++ r.tensors;

  top.subed = 
    zip3(
      \ sbs::[String] sbd::TensorExpr avail::Boolean ->
        if avail
        then 
          if null(sbs)
          then sbd
          else 
            tensorBaseExpr(
              declRefExpr(
                name(head(sbs), location=top.location),
                location=top.location
              ),
              env,
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );

  l.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail
        then []
        else take(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  r.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[TensorExpr] ->
        if avail 
        then []
        else drop(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);

  r.tensorNames = 
    drop(listLength(l.tensors), top.tensorNames);

  top.compute = generateCompute(top);
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

function generateCompute
String ::= ex::TensorExpr
{
  return 
    generateCompute_helper(
      ex.conds, ex.sparse, ex.dense, 
      ex.canSub, ex.subed, ex.accessOrder
    );
}

function generateCompute_helper
String ::= 
  conds::[TensorCond] sparse::[[Pair<String Integer>]] dense::[[Pair<String Integer>]]
  canSub::[[TensorExpr]] subed::[TensorExpr] access::[String]
{
  local c::TensorCond = head(conds);
  local s::[Pair<String Integer>] = head(sparse);
  local d::[Pair<String Integer>] = head(dense);
  local cs::[TensorExpr] = head(canSub);
  local sd::TensorExpr = head(subed);
  local iv::String = head(access);

  return
    if null(conds)
    then ""
    else
      case c of
      | sparseAccess(_, _) -> ""
      | denseAccess(_, _, _) -> ""
      | _ ->
        implode("\n",
          map(
            \ p::Pair<String Integer> ->
              s"unsigned long p${p.fst}${toString(p.snd+1)} = ${p.fst}${toString(p.snd+1)}_pos[${if p.snd == 0 then "0" else s"p${p.fst}${toString(p.snd)}"}];"
            ,
            s
          )
        )
      end
      ++
      -- Something else has to happen here, need to handle lattice correctly
      case c of
      | sparseAccess(tNm, d) ->
        s"for(unsigned long p${tNm}${toString(d+1)} = ${tNm}${toString(d+1)}_pos[${if d == 0 then "0" else s"p${tNm}${toString(d)}"}]; p${tNm}${toString(d+1)} < ${tNm}${toString(d+1)}_pos[${if d == 0 then "1" else s"p${tNm}${toString(d)} + 1"}]; p${tNm}${toString(d+1)})"
      | denseAccess(tNm, d, var) ->
        s"for(unsigned long ${var} = 0; ${var} < ${tNm}${toString(d+1)}_dimension; ${var}++)"
      | _ -> 
        s"while(${c.condition})"
      end
      ++
      "{"
      ++
      generateCompute_helper(
        tail(conds), tail(sparse), tail(dense),
        tail(canSub), tail(subed), tail(access)
      )
      ++
      "}";
}
