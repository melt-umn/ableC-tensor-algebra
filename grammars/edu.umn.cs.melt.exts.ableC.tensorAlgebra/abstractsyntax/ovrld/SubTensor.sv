grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production subTensor
top::Expr ::= l::Expr r::Expr
{
  top.tensorName = "";

  top.conds =
    zipWith(
      \ cl::TensorCond cr::TensorCond
      -> condAnd(cl, cr)
      ,
      l.conds,
      r.conds
    );

  top.subExpr =
    zipWith(
      \ sl::[Expr] sr::[Expr]
      -> l :: r ::
         map(
           \ e::Expr
           -> subTensor(e, r, location=top.location)
           ,
           sl
         )
         ++
         map(
           \ e::Expr
           -> subTensor(l, e, location=top.location)
           ,
           sr
         )
      ,
      l.subExpr,
      r.subExpr
    );

  top.sparse =
    zipWith(
      \ ll::[Pair<Expr Integer>] lr::[Pair<Expr Integer>]
      -> 
        nubBy(
          \ p1::Pair<Expr Integer> p2::Pair<Expr Integer>
          -> let e1::Decorated Expr =
               decorate p1.fst with {env=top.env; returnType=top.returnType;}
             in let e2::Decorated Expr =
               decorate p2.fst with {env=top.env; returnType=top.returnType;}
             in
             e1.tensorName == e2.tensorName
             && p1.snd == p2.snd
             end
             end
          ,
          ll ++ lr
        )
      ,
      l.sparse,
      r.sparse
    );

  top.isAvail = 
    zipWith(
      \ bl::Boolean br::Boolean
      -> bl && br
      ,
      l.isAvail,
      r.isAvail
    );

  top.canSub = 
    zip3(
      \ ls::Pair<[Expr] [Expr]> avail::Boolean availLast::Boolean
      -> if avail && !availLast
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

  top.orders =
    l.orders ++ r.orders;

  top.tensors =
    l.tensors ++ r.tensors;
  
  l.tensorNames =
    take(listLength(l.tensors), top.tensorNames);
  
  r.tensorNames =
    drop(listLength(l.tensors), top.tensorNames);

  l.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[Expr]
      -> if avail
         then []
         else take(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );
  
  r.subNames =
    zip3(
      \ avail::Boolean sbs::[String] exs::[Expr]
      -> if avail
         then []
         else drop(listLength(exs), sbs)
      ,
      top.isAvail,
      top.subNames,
      l.canSub
    );

  top.subed = 
    zip3(
      \ exs::Pair<Expr Expr> avail::Boolean nms::[String]
      -> if avail
         then 
           declRefExpr(
             name(
               head(nms), 
               location=top.location
             ), 
             location=top.location
           )
         else 
           subTensor(exs.fst, exs.snd, location=top.location)
      ,
      zipWith(pair, l.subed, r.subed),
      top.isAvail,
      top.subNames
    );

  forwards to 
    mkErrorCheck(
      l.errors ++ r.errors,
      emptyAccess
    );
}
