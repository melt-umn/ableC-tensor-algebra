grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

synthesized attribute conds :: [TensorCond];
synthesized attribute subExpr :: [[Expr]];
synthesized attribute sparse :: [[Pair<String Integer>]];
synthesized attribute canSub :: [[Expr]];
synthesized attribute subNames :: [[String]];
synthesized attribute output :: Expr;
synthesized attribute orders :: [[String]];

attribute conds, subExpr, sparse, canSub, subNames, output, orders, output, orders occurs on Expr;
