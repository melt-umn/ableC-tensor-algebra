grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

synthesized attribute tensorName :: String;
synthesized attribute conds :: [TensorCond];
synthesized attribute subExpr :: [[Expr]];
synthesized attribute sparse :: [[Pair<Expr Integer>]];
synthesized attribute canSub :: [[Expr]];
synthesized attribute isAvail :: [Boolean];
synthesized attribute orders :: [[String]];
synthesized attribute tensors :: [Expr];
synthesized attribute subed :: [Expr];

autocopy attribute accessOrder :: [String];
autocopy attribute subNames :: [[String]];
autocopy attribute tensorNames :: [String];
autocopy attribute output :: Expr;

attribute 
  tensorName,
  conds,
  subExpr,
  sparse,
  canSub,
  isAvail,
  orders,
  tensors,
  subed,
  accessOrder,
  subNames,
  tensorNames,
  output
occurs on 
  Expr;

aspect default production
top::Expr ::=
{
  top.tensorName = "";

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
