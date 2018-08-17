grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:halide;

abstract production halideTensorCompute
top::Stmt ::= tns::Expr idx::Expr val::Expr ts::Transformation
{
  forwards to
    halideSetup(
      tns, idx, val,
      iterateStmt(
        halideTensorExpr(tns, idx, val),
        ts
      )
    );
}

abstract production halideTensorComputeOrdered
top::Stmt ::= tns::Expr idx::Expr val::Expr ord::[String] ts::Transformation
{
  forwards to
    halideSetup(
      tns, idx, val,
      iterateStmt(
        halideTensorExprOrder(tns, idx, val, ord),
        ts
      )
    );
}

abstract production halideScalarCompute
top::Stmt ::= nm::Name val::Expr ts::Transformation
{
  forwards to
    halideScalarSetup(
      nm, val,
      iterateStmt(
        halideScalarTensorExpr(nm, val),
        ts
      )
    );
}

abstract production halideScalarComputeOrdered
top::Stmt ::= nm::Name val::Expr ord::[String] ts::Transformation
{
  forwards to
    halideScalarSetup(
      nm, val,
      iterateStmt(
        halideScalarExprOrder(nm, val, ord),
        ts
      )
    );
}
