grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

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

autocopy attribute accessOrder :: [String];
autocopy attribute subNames :: [[String]];
autocopy attribute tensorNames :: [String];
autocopy attribute output :: TensorExpr;

nonterminal TensorExpr with
  tensorName, tensorExpr, conds, subExpr,
  sparse, dense, canSub, isAvail, orders,
  tensors, subed, accessOrder, subNames,
  tensorNames, output, location;

abstract production tensorBaseExpr
top::TensorExpr ::= ex::Expr
{
  top.tensorName = "__none";
  
  top.tensorExpr = ex;
  
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
              location=top.location
            )
        else top
      ,
      top.subNames,
      top :: top.subed,
      top.isAvail
    );
}

abstract production tensorAdd
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr
{
  top.tensorName = "__name";

  top.tensorExpr = ex;

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
}

abstract production tensorSub
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr
{
  top.tensorName = "__name";

  top.tensorExpr = ex;

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
}

abstract production tensorMul
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr
{
  top.tensorName = "__none";

  top.tensorExpr = ex;

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
}

abstract production tensorDiv
top::TensorExpr ::= ex::Expr l::TensorExpr r::TensorExpr
{
  top.tensorName = "__none";

  top.tensorExpr = ex;

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
