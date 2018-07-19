grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

global emptyAccess :: Expr =
  parseExpr(s"""
  ({
    struct tensor_acc x;
    x;
  })
  """);
