grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute exprs :: TensorAssignExpr;
synthesized attribute conds :: TensorCond;
synthesized attribute points :: [LatticePoints];

nonterminal MergeLattice with points;
nonterminal LatticePoints with points, exprs, conds;

abstract production nullLattice
top::MergeLattice ::=
{
  top.points = [];
}

abstract production builtLattice
top::MergeLattice ::= points::[LatticePoints]
{
  top.points = points;
}


abstract production nullPoint
top::LatticePoints ::= loc::Location
{
  top.points = [];
  top.exprs = nullAssignExpr(location=loc);
  top.conds = nullCond();
}

abstract production builtPoint
top::LatticePoints ::= points::[LatticePoints] exprs::TensorAssignExpr conds::TensorCond
{
  top.points = points;
  top.exprs = exprs;
  top.conds = conds;
}

function merge_lattice
MergeLattice ::= expr::TensorAssignExpr var::String loc::Location
{
  return builtLattice([lattice_points(expr, var, loc)]);
}

function lattice_points
LatticePoints ::= expr::TensorAssignExpr var::String loc::Location
{
    return case expr.tensorValue of
           | nullTensorExpr() -> nullPoint(loc)
           | access(nm, acc) -> 
               let index::Integer = positionOf(\s::String v::String -> s == v, var, acc)
               in
               if index == -1
               then nullPoint(loc)
               else builtPoint([], expr, accessCond(nm, index))
               end
           | tExpr(exp) -> builtPoint([], expr, allCond())
           | add(l, r) -> 
               case expr of
               | assignExpr(b, acc, _, ts, fs) ->
                   let lP::LatticePoints = 
                       lattice_points(
                         assignExpr(b, acc, l, ts, fs, location=loc),
                         var, 
                         loc)
                   in let rP::LatticePoints = 
                          lattice_points(
                            assignExpr(b, acc, r, ts, fs, location=loc), 
                            var, 
                            loc)
                   in builtPoint(
                        map(pointAdd(_, r, 0, var, loc),
                          lP.points)
                        ++
                        map(pointAdd(_, l, 1, var, loc),
                          rP.points)
                        ++ (lP :: rP :: [])
                        , 
                        expr, orCond(lP.conds, rP.conds))
                   end
                   end
               | _ -> nullPoint(loc)
               end
           | mul(l, r) -> 
               case expr of
               | assignExpr(b, acc, _, ts, fs) ->
                   let lP::LatticePoints = 
                       lattice_points(
                         assignExpr(b, acc, l, ts, fs, location=loc), 
                         var, 
                         loc)
                   in let rP::LatticePoints = 
                          lattice_points(
                            assignExpr(b, acc, r, ts, fs, location=loc), 
                            var, 
                            loc)
                   in builtPoint(
                        map(pointMult(_, r, 0, var, loc),
                          lP.points)
                        ++ 
                        map(pointMult(_, l, 1, var, loc),
                          rP.points)
                        ,
                        expr, andCond(lP.conds, rP.conds))
                   end
                   end
               | _ -> nullPoint(loc)
               end
           end;
}

function pointMult
LatticePoints ::= pnt::LatticePoints expr::TensorExpr lc::Integer var::String loc::Location
{
  return
    case lc of
    | 0 -> -- expr is on the right
        builtPoint(
          map(pointMult(_, expr, lc, var, loc), pnt.points),
          assignExprExpr(
            pnt.exprs.tensorAssign,
            mul(pnt.exprs.tensorValue, expr, location=loc),
            pnt.exprs.tensorFormat,
            location = loc
          ),
          andCond(pnt.conds, generateCond(expr, var, loc))
        )
    | 1 -> -- expr is on the left
        builtPoint(
          map(pointMult(_, expr, lc, var, loc), pnt.points),
          assignExprExpr(
            pnt.exprs.tensorAssign,
            mul(expr, pnt.exprs.tensorValue, location=loc),
            pnt.exprs.tensorFormat,
            location = loc
          ), 
          andCond(generateCond(expr, var, loc), pnt.conds)
        )
    | _ -> nullPoint(loc)
    end;
}

function pointAdd
LatticePoints ::= pnt::LatticePoints expr::TensorExpr lc::Integer var::String loc::Location
{
  return
    case lc of
    | 0 -> -- expr is on the right
        builtPoint(
          pnt :: 
          lattice_points(
            assignExprExpr(
              pnt.exprs.tensorAssign,
              expr,
              pnt.exprs.tensorFormat,
              location=loc
            ), 
            var, 
            loc
          ) ::
          map(pointAdd(_, expr, lc, var, loc), pnt.points)
          ,
          assignExprExpr(
            pnt.exprs.tensorAssign,
            add(pnt.exprs.tensorValue, expr, location=loc),
            pnt.exprs.tensorFormat,
            location=loc
          ),
          orCond(pnt.conds, generateCond(expr, var, loc))
        )
    | 1 -> -- expr is on the left
        builtPoint(
          pnt :: 
          lattice_points(
            assignExprExpr(
              pnt.exprs.tensorAssign,
              expr,
              pnt.exprs.tensorFormat,
              location=loc
            ),
            var,
            loc
          ) ::
          map(pointAdd(_, expr, lc, var, loc), pnt.points)
          ,
          assignExprExpr(
            pnt.exprs.tensorAssign,
            add(expr, pnt.exprs.tensorValue, location=loc),
            pnt.exprs.tensorFormat,
            location = loc
          ),
          orCond(generateCond(expr, var, loc), pnt.conds)
        )
    | _ -> nullPoint(loc)
    end;
}

function generateCond
TensorCond ::= expr::TensorExpr var::String loc::Location
{
  return case expr of
         | nullTensorExpr() -> nullCond()
         | access(nm, acc) -> 
             let index::Integer = 
               positionOf(\s::String v::String -> s == v,
                          var, 
                          acc)
             in if index == -1
             then nullCond()
             else accessCond(nm, index)
             end
         | tExpr(expr) -> allCond()
         | add(l, r) ->
             let lC::TensorCond =
               generateCond(l, var, loc)
             in let rC::TensorCond =
               generateCond(r, var, loc)
             in orCond(lC, rC)
             end
             end
         | mul(l, r) ->
             let lC :: TensorCond =
               generateCond(l, var, loc)
             in let rC :: TensorCond =
               generateCond(r, var, loc)
             in andCond(lC, rC)
             end
             end
         | _ -> nullCond()
         end;
}

function lattice_optimize
MergeLattice ::= lat::MergeLattice formats:: tm:Map<Name TensorFormatItem>
{
  return builtLattice(map(points_optimize(_, formats), lat.points));
}

function points_optimize
LatticePoints ::= p::LatticePoints formats :: tm:Map<Name TensorFormatItem>
{
  local pnts::[LatticePoints] = map(points_optimize(_, formats), p.points);
  local expr::TensorAssignExpr = p.exprs;
  local cond::TensorCond = cond_optimize(p.conds, formats);

  local ps::[LatticePoints] = 
    filter(
      \pn::LatticePoints ->
        !condEqual(pn.conds, cond)
      , 
      pnts);
  
  local children::[LatticePoints] =
    child_points(ps);
  
  local chs::[LatticePoints] =
    filter(
      \ p::LatticePoints ->
      !containsBy(
        \ a::LatticePoints
          b::LatticePoints
       -> condEqual(a.conds, b.conds),
        p,
        children
      ),
      ps
    );
  
  return builtPoint(chs, expr, cond);
}

function child_points
[LatticePoints] ::= ps::[LatticePoints]
{
  return 
    flatMap(
      \p::LatticePoints ->
        p.points ++ child_points(p.points)
      ,
      ps
    );
}

function cond_optimize
TensorCond ::= c::TensorCond fmt:: tm:Map<Name TensorFormatItem>
{
  return
    if isNullCond(c) || isAllCond(c)
    then c
    else if isAccessCond(c)
    then case head(c.tensorElems) of
         | left(tn) -> 
             let format::[TensorFormatItem] = tm:lookup(tn, fmt)
             in if null(format)
             then nullCond()
             else let form::TensorFormatItem = head(format)
             in let type::Integer = case getElem(form.specifiers, c.tensorDim) of
                                    | nothing() -> 0
                                    | just(i) -> i
                                    end
             in if type == storeDense
             then allCond()
             else c
             end
             end
             end
         | _ -> nullCond()
         end
    else if isAndCond(c)
    then case head(c.tensorElems), head(tail(c.tensorElems)) of
         | right(l), right(r) -> 
             let oL::TensorCond = cond_optimize(l, fmt)
             in let oR::TensorCond = cond_optimize(r, fmt)
             in andCond(oL, oR)
             end
             end
         | _, _ -> nullCond()
         end
    else if isOrCond(c)
    then case head(c.tensorElems), head(tail(c.tensorElems)) of
         | right(l), right(r) -> 
             let oL::TensorCond = cond_optimize(l, fmt)
             in let oR::TensorCond = cond_optimize(r, fmt)
             in orCond(oL, oR)
             end
             end
         | _, _ -> nullCond()
         end
    else nullCond();
}

function sparse_dimensions
[Pair<String Integer>] ::= lat::MergeLattice var::String
{
  return 
    nubBy(
      \ a::Pair<String Integer>
        b::Pair<String Integer>
     -> a.fst == b.fst && a.snd == b.snd
      ,
      flatMap(sparse_dim(_, var), lat.points)
    );
}

function sparse_dim
[Pair<String Integer>] ::= pnt::LatticePoints var::String
{
  return expr_sparse(pnt.exprs, var) ++
    flatMap(sparse_dim(_, var), pnt.points);
}

function expr_sparse
[Pair<String Integer>] ::= exp::TensorAssignExpr var::String
{
  return exp_sparse(exp.tensorValue, exp.tensorFormat, var);
}

function exp_sparse
[Pair<String Integer>] ::= exp::TensorExpr fmt::tm:Map<Name TensorFormatItem> var::String
{
  return
    case exp of
    | access(nm, acc) -> 
        let i::Integer =
          positionOf(
            \ s1::String
              s2::String
           -> s1 == s2
            , 
            var, 
            acc
          )
        in
        if i == -1
        then []
        else let fms::[TensorFormatItem] = 
             tm:lookup(nm, fmt)
             in
             if null(fms)
             then []
             else let fm::TensorFormatItem =
                    head(fms)
             in
             if case getElem(fm.specifiers, i) of
                | nothing() -> false
                | just(i) -> i  == storeDense
                end
             then []
             else let j::Integer = 
                     positionOf(
                       \ x::Integer
                         y::Integer
                      -> x == y
                       ,
                       i,
                       fm.dimenOrder
                     )
             in pair(nm.name, j+1) :: []
             end
             end
             end
        end
    | add(l, r) ->
        exp_sparse(l, fmt, var) ++ exp_sparse(r, fmt, var)
    | mul(l, r) ->
        exp_sparse(l, fmt, var) ++ exp_sparse(r, fmt, var)
    | _ -> []
    end;
}

function dense_dimensions
[Pair<String Integer>] ::= lat::MergeLattice var::String
{
  return
    nubBy(
      \ a::Pair<String Integer>
        b::Pair<String Integer>
      -> a.fst == b.fst && a.snd == b.snd
      ,
      flatMap(dense_dim(_, var), lat.points)
    );
}

function dense_dim
[Pair<String Integer>] ::= pnt::LatticePoints var::String
{
  return expr_dense(pnt.exprs, var) ++
    flatMap(dense_dim(_, var), pnt.points);
}

function expr_dense
[Pair<String Integer>] ::= expr::TensorAssignExpr var::String
{
  return exp_dense(expr.tensorValue, expr.tensorFormat, var) ++
         exp_dense(expr.tensorAssign, expr.tensorFormat, var);
}

function exp_dense
[Pair<String Integer>] ::= exp::TensorExpr fmt::tm:Map<Name TensorFormatItem> var::String
{
  return
    case exp of
    | access(nm, acc) -> 
        let i::Integer =
          positionOf(
            \ s1::String
              s2::String
            -> s1 == s2
            ,
            var,
            acc
          )
        in
        if i == -1
        then []
        else let fms::[TensorFormatItem] =
             tm:lookup(nm, fmt)
             in
             if null(fms)
             then []
             else let fm::TensorFormatItem = 
                    head(fms)
             in
             if case getElem(fm.specifiers, i) of
                | nothing() -> false
                | just(i) -> i == storeDense
                end
             then let j::Integer =
                    positionOf(
                      \ x::Integer
                        y::Integer
                      -> x == y
                      ,
                      i,
                      fm.dimenOrder
                    )
              in pair(nm.name, j+1) :: []
              end
             else []
             end
             end
        end
    | add(l, r) ->
        exp_dense(l, fmt, var) ++ exp_dense(r, fmt, var)
    | mul(l, r) ->
        exp_dense(l, fmt, var) ++ exp_dense(r, fmt, var)
    | _ -> []
    end;
}

function merged_dimensions
[Pair<String Integer>] ::= pnt::LatticePoints fmts::tm:Map<Name TensorFormatItem>
{
  return merged_dimensions_cond(pnt.conds, fmts);
}

function merged_dimensions_cond
[Pair<String Integer>] ::= cnd::TensorCond fmts::tm:Map<Name TensorFormatItem>
{
  return
    if isNullCond(cnd) || isAllCond(cnd)
    then []
    else if isAccessCond(cnd)
    then case head(cnd.tensorElems) of
         | left(nm) -> 
             let fmt::TensorFormatItem =
               head(tm:lookup(nm, fmts))
             in let d::Integer=
               positionOf(
                 \ x::Integer
                   y::Integer
                 -> x == y
                 ,
                 cnd.tensorDim,
                 fmt.dimenOrder
               )
             in pair(nm.name, d) :: []
             end
             end
         | _ -> []
         end
    else case head(cnd.tensorElems), head(tail(cnd.tensorElems)) of
         | right(c1), right(c2) ->
             merged_dimensions_cond(c1, fmts) ++
             merged_dimensions_cond(c2, fmts)
         | _, _ -> []
         end;
}

function sub_points
[LatticePoints] ::= p::LatticePoints
{
  return 
    nubBy(
      \ a::LatticePoints
        b::LatticePoints
      -> pointsEqual(a, b)
      ,
      child_points([p])
    );
}

function pointsEqual
Boolean ::= p1::LatticePoints p2::LatticePoints
{
  return condEqual(p1.conds, p2.conds);
}
