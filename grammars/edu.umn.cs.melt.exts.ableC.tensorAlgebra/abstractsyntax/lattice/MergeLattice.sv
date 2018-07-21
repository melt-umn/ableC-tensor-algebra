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
LatticePoints ::= 
  assign::TensorExpr fmts::tm:Map<String TensorFormat> value::TensorExpr
  var::String loc::Location env::Decorated Env
{
  return
    case env of
    | 
}
