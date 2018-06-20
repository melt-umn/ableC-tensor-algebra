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
                   in builtPoint([lP, rP], expr, orCond(lP.conds, rP.conds))
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
  
  return builtPoint(ps, expr, cond);
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
             in let type::Integer = getElem(form.specifiers, c.tensorDim)
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
[Pair<String Integer>] ::= lat::MergeLattice fmts :: tm:Map<Name TensorFormatItem>
{
  return [];
}
