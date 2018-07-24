grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:lattice;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute value :: TensorExpr;
synthesized attribute cond :: TensorCond;
synthesized attribute pnts :: [LatticePoint];

autocopy attribute fmts :: tm:Map<String TensorFormat>;
autocopy attribute assign :: TensorExpr;

nonterminal LatticePoint with value, assign, fmts, cond, pnts;

abstract production latticePoint
top::LatticePoint ::= pnts::[LatticePoint] value::TensorExpr cond::TensorCond
{
  top.value = value;
  top.pnts = pnts;
  top.cond = cond;
}

function lattice_points
LatticePoint ::= 
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  var::String loc::Location env::Decorated Env loop::Boolean
{
  value.fmts = fmts;

  return
    case value of
    | tensorBaseExpr(_, _) -> latticePoint([], value, allCond(var))
    | tensorAccess(_, _, _, _) -> 
      let i::Integer =
        positionOf(stringEq, var, head(value.accesses))
      in
      let f::TensorFormat =
        head(tm:lookup(value.tensorName, fmts))
      in
      if i == -1
      then latticePoint([], value, nullCond())
      else latticePoint([], value, accessCond(value.tensorName, i, var, f))
      end
      end
    | tensorAdd(_, l, r, _) ->
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
            lP.pnts)
          ++
          map(pointAdd(_, assign, fmts, l, 1, var, loc, env, loop),
            rP.pnts)
          ++ (lP :: rP :: [])
          ,
          value, condOr(lP.cond, rP.cond, loop))
      end
      end
      end
    | tensorSub(_, l, r, _) -> 
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
    | tensorMul(_, l, r, _) -> 
      let lP::LatticePoint =
        lattice_points(assign, fmts, l, var, loc, env, loop)
      in let rP::LatticePoint =
        lattice_points(assign, fmts, r, var, loc, env, loop)
      in
      latticePoint(
        map(pointMul(_, assign, fmts, r, 0, var, loc, env, loop),
          lP.pnts)
        ++
        map(pointMul(_, assign, fmts, l, 1, var, loc, env, loop),
          rP.pnts)
        ,
        value, condAnd(lP.cond, rP.cond, loop))
      end
      end
    | tensorDiv(_, l, r, _) ->
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
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        pnt ::
        lattice_points(
          assign,
          fmts,
          expr,
          var,
          loc,
          env, loop
        ) ::
        map(pointAdd(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts)
        ,
        tensorAdd(addExpr(pnt.value.tensorExpr, expr.tensorExpr, location=loc), pnt.value, expr, env, location=loc),
        condOr(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        pnt ::
        lattice_points(
          assign,
          fmts,
          expr,
          var,
          loc,
          env, loop
        ) ::
        map(pointAdd(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts)
        ,
        tensorAdd(addExpr(expr.tensorExpr, pnt.value.tensorExpr, location=loc), expr, pnt.value, env, location=loc),
        condOr(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    end;
}

function pointSub
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        pnt ::
        lattice_points(
          assign,
          fmts,
          expr,
          var,
          loc,
          env, loop
        ) ::
        map(pointSub(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts)
        ,
        tensorSub(subExpr(pnt.value.tensorExpr, expr.tensorExpr, location=loc), pnt.value, expr, env, location=loc),
        condOr(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        pnt ::
        lattice_points(
          assign,
          fmts,
          expr,
          var,
          loc,
          env, loop
        ) ::
        map(pointSub(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts)
        ,
        tensorSub(subExpr(expr.tensorExpr, pnt.value.tensorExpr, location=loc), expr, pnt.value, env, location=loc),
        condOr(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    end;
}

function pointMul
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        map(pointMul(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts),
        tensorMul(mulExpr(pnt.value.tensorExpr, expr.tensorExpr, location=loc), pnt.value, expr, env, location=loc),
        condAnd(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        map(pointMul(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts),
        tensorMul(mulExpr(expr.tensorExpr, pnt.value.tensorExpr, location=loc), expr, pnt.value, env, location=loc),
        condAnd(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    end;
}

function pointDiv
LatticePoint ::= 
  pnt::LatticePoint assign::TensorExpr fmts::tm:Map<String TensorFormat> 
  expr::TensorExpr lc::Integer var::String loc::Location env::Decorated Env loop::Boolean
{
  return
    case lc of
    | 0 -> -- expr is on the right
      latticePoint(
        map(pointDiv(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts),
        tensorDiv(divExpr(pnt.value.tensorExpr, expr.tensorExpr, location=loc), pnt.value, expr, env, location=loc),
        condAnd(pnt.cond, generateCond(expr, var, loc, fmts, loop), loop)
      )
    | 1 -> -- expr is on the left
      latticePoint(
        map(pointDiv(_, assign, fmts, expr, lc, var, loc, env, loop), pnt.pnts),
        tensorDiv(divExpr(expr.tensorExpr, pnt.value.tensorExpr, location=loc), expr, pnt.value, env, location=loc),
        condAnd(generateCond(expr, var, loc, fmts, loop), pnt.cond, loop)
      )
    end;
}

function generateCond
TensorCond ::= 
  expr::TensorExpr var::String loc::Location fmts::tm:Map<String TensorFormat> loop::Boolean
{
  expr.fmts = fmts;

  return
    case expr of
    | tensorBaseExpr(_, _) -> allCond(var)
    | tensorAccess(_, _, _, _) ->
      let i::Integer = 
        positionOf(stringEq, var, head(expr.accesses))
      in
      let f::TensorFormat = 
        head(tm:lookup(expr.tensorName, fmts))
      in
      if i == -1
      then nullCond()
      else accessCond(expr.tensorName, i, var, f)
      end
      end
    | tensorAdd(_, l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condOr(lC, rC, loop)
      end
      end
    | tensorSub(_, l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condOr(lC, rC, loop)
      end
      end
    | tensorMul(_, l, r, _) ->
      let lC::TensorCond =
        generateCond(l, var, loc, fmts, loop)
      in let rC::TensorCond =
        generateCond(r, var, loc, fmts, loop)
      in
      condAnd(lC, rC, loop)
      end
      end
    | tensorDiv(_, l, r, _) ->
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
