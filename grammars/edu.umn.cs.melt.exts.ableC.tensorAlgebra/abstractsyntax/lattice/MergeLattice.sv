grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Our representation of the merge lattices introduced by Kjolstad.
   They represent where multiple tensors are merged, and (if addition
   or subtraction is used) show how we handle emitting partial 
   expressions (e.g. A[i] = B[i] + C[i] becoming A[i] = C[[i] if
   B[i] is zero (it is sparse and doesn't exist)) -}

synthesized attribute value :: TensorExpr; -- a TensorExpr of the rhs
synthesized attribute cond :: TensorCond; -- the condition for this merge
synthesized attribute pnts :: [LatticePoint]; -- subpoints

autocopy attribute fmts :: tm:Map<String TensorFormat>;

nonterminal LatticePoint with value, fmts, cond, pnts;

abstract production latticePoint
top::LatticePoint ::= pnts::[LatticePoint] value::TensorExpr cond::TensorCond
{
  top.value = value;
  top.pnts = pnts;
  top.cond = cond;
}

{- Build a merge lattice for a tensor expression, at a specific variable in
   the process -}
function lattice_points
LatticePoint ::= 
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  var::String loc::Location env::Decorated Env loop::Boolean
{
  value.fmts = fmts;

  return
    case value of
    | tensorBaseExpr(_, _) -> latticePoint([], value, allCond(var))
    | tensorAccess(_, _, _) -> 
      let i::Integer =
        positionOf(var, head(value.accesses))
      in
      let f::TensorFormat =
        head(tm:lookup(value.tensorName, fmts))
      in
      if i == -1
      then latticePoint([], value, nullCond()) -- tensor not accessed by this var
      else latticePoint([], value, accessCond(value.tensorName, i, var, f))
      end
      end
    | tensorAdd(l, r, _) ->
      let lP::LatticePoint =
        lattice_points(assign, fmts, l, var, loc, env, loop)
      in let rP::LatticePoint =
        lattice_points(assign, fmts, r, var, loc, env, loop)
      in
      case lP.cond, rP.cond of
      | nullCond(), nullCond() -> 
        latticePoint([], value, nullCond())
      | nullCond(), _ ->
        latticePoint(rP.pnts, value, rP.cond)
      | _, nullCond() -> 
        latticePoint(lP.pnts, value, lP.cond)
      | _, _ ->
        latticePoint(
          map(pointAdd(_, assign, fmts, r, 0, var, loc, env, loop),
            lP.pnts) -- Subpoints include pnts of left added to right
          ++
          map(pointAdd(_, assign, fmts, l, 1, var, loc, env, loop),
            rP.pnts) -- subpoints include pnts of right added to left
          ++ (lP :: rP :: []) -- include left and right and their points
          ,
          value, condOr(lP.cond, rP.cond, loop))
      end
      end
      end
    | tensorSub(l, r, _) -> -- same as addition
      let lP::LatticePoint =
        lattice_points(assign, fmts, l, var, loc, env, loop)
      in let rP::LatticePoint =
        lattice_points(assign, fmts, r, var, loc, env, loop)
      in
      case lP.cond, rP.cond of
      | nullCond(), nullCond() -> 
        latticePoint([], value, nullCond())
      | nullCond(), _ ->
        latticePoint(rP.pnts, value, rP.cond)
      | _, nullCond() -> 
        latticePoint(lP.pnts, value, lP.cond)
      | _, _ ->
        latticePoint(
          map(pointSub(_, assign, fmts, r, 0, var, loc, env, loop),
            lP.pnts)
          ++
          map(pointSub(_, assign, fmts, l, 1, var, loc, env, loop),
            rP.pnts)
          ++ (lP :: rP :: [])
          ,
          value, condOr(lP.cond, rP.cond, loop))
      end
      end
      end
    | tensorMul(l, r, _) ->
      let lP::LatticePoint =
        lattice_points(assign, fmts, l, var, loc, env, loop)
      in let rP::LatticePoint =
        lattice_points(assign, fmts, r, var, loc, env, loop)
      in -- Condition is easier, no checking for null, just and cond
      latticePoint(
        map(pointMul(_, assign, fmts, r, 0, var, loc, env, loop),
          lP.pnts)
        ++
        map(pointMul(_, assign, fmts, l, 1, var, loc, env, loop),
          rP.pnts)
        , -- Don't add lP and rP to pnts, because if only 1 exists, this is zero
        value, condAnd(lP.cond, rP.cond, loop))
      end
      end
    | tensorDiv(l, r, _) -> -- Same as multiplication
      let lP::LatticePoint =
        lattice_points(assign, fmts, l, var, loc, env, loop)
      in let rP::LatticePoint =
        lattice_points(assign, fmts, r, var, loc, env, loop)
      in
      latticePoint(
        map(pointDiv(_, assign, fmts, r, 0, var, loc, env, loop),
          lP.pnts)
        ++
        map(pointDiv(_, assign, fmts, l, 1, var, loc, env, loop),
          rP.pnts)
        ,
        value, condAnd(lP.cond, rP.cond, loop))
      end
      end
    end;
}

function pointAdd
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env 
  loop::Boolean
{
  return
    case lc of -- lc tells us whether this point is the rhs or lhs os the addition
    | 0 -> -- expr is on the right
      latticePoint(
        let osd::LatticePoint = -- the point for the other side
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointAdd(_, assign, fmts, expr, 0, var, loc, env, loop), pnt.pnts)
        ++ -- ^ add lhs to all points below pnt
        map(pointAdd(_, pnt.value, fmts, expr, 1, var, loc, env, loop), osd.pnts)
        ++ -- ^ add rhs to all points below lhs
        (pnt :: -- in addition, each side of the operation is a subpnt
         osd :: [])
        end,
        tensorAdd(pnt.value, expr, env, location=loc),
        condOr(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointAdd(_, assign, fmts, expr, 1, var, loc, env, loop), pnt.pnts)
        ++
        map(pointAdd(_, pnt.value, fmts, expr, 0, var, loc, env, loop), osd.pnts)
        ++
        (pnt :: osd :: [])
        end,
        tensorAdd(expr, pnt.value, env, location=loc),
        condOr(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    | _ -> error("lc must be 0 or 1")
    end;
}

function pointSub
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env 
  loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointSub(_, assign, fmts, expr, 0, var, loc, env, loop), pnt.pnts)
        ++
        map(pointSub(_, pnt.value, fmts, expr, 1, var, loc, env, loop), osd.pnts)
        ++
        (pnt :: osd :: [])
        end,
        tensorSub(pnt.value, expr, env, location=loc),
        condOr(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointSub(_, assign, fmts, expr, 1, var, loc, env, loop), pnt.pnts)
        ++
        map(pointSub(_, pnt.value, fmts, expr, 0, var, loc, env, loop), osd.pnts)
        ++
        (pnt :: osd :: [])
        end,
        tensorSub(expr, pnt.value, env, location=loc),
        condOr(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    | _ -> error("lc must be 0 or 1")
    end;
}

function pointMul
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env 
  loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var, 
            loc, env, loop
          )
        in
        map(pointMul(_, assign, fmts, expr, 0, var, loc, env, loop), pnt.pnts)
        ++
        map(pointMul(_, pnt.value, fmts, expr, 1, var, loc, env, loop), osd.pnts)
        end,
        tensorMul(pnt.value, expr, env, location=loc),
        condAnd(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointMul(_, assign, fmts, expr, 1, var, loc, env, loop), pnt.pnts)
        ++
        map(pointMul(_, pnt.value, fmts, expr, 0, var, loc, env, loop), osd.pnts)
        end,
        tensorMul(expr, pnt.value, env, location=loc),
        condAnd(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    | _ -> error("lc must be 0 or 1")
    end;
}

function pointDiv
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env 
  loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointDiv(_, assign, fmts, expr, 0, var, loc, env, loop), pnt.pnts)
        ++
        map(pointDiv(_, pnt.value, fmts, expr, 1, var, loc, env, loop), osd.pnts)
        end,
        tensorDiv(pnt.value, expr, env, location=loc),
        condAnd(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        let osd::LatticePoint =
          lattice_points(
            assign, fmts, expr, var,
            loc, env, loop
          )
        in
        map(pointDiv(_, assign, fmts, expr, 1, var, loc, env, loop), pnt.pnts)
        ++
        map(pointDiv(_, pnt.value, fmts, expr, 0, var, loc, env, loop), osd.pnts)
        end,
        tensorDiv(expr, pnt.value, env, location=loc),
        condAnd(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    | _ -> error("lc must be 0 or 1")
    end;
}

function generateCond
TensorCond ::= 
  expr::TensorExpr var::String loc::Location fmts::tm:Map<String TensorFormat> 
  loop::Boolean
{
  expr.fmts = fmts;

  return
    case expr of
    | tensorBaseExpr(_, _) -> allCond(var)
    | tensorAccess(_, _, _) ->
      let i::Integer = 
        positionOf(var, head(expr.accesses))
      in
      let f::TensorFormat = 
        head(tm:lookup(expr.tensorName, fmts))
      in
      if i == -1
      then nullCond()
      else accessCond(expr.tensorName, i, var, f)
      end
      end
    | tensorAdd(l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condOr(lC, rC, loop)
      end
      end
    | tensorSub(l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condOr(lC, rC, loop)
      end
      end
    | tensorMul(l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condAnd(lC, rC, loop)
      end
      end
    | tensorDiv(l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condAnd(lC, rC, loop)
      end
      end
    end;
}
