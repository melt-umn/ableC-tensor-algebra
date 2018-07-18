grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

aspect function ovrld:getCallOverloadProd
Maybe<(Expr ::= Expr Exprs Location)> ::= t::Type env::Decorated Env
{
  overloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor",
      accessTensor(_, _, env, location=_)
    )];
}

aspect function ovrld:getAddOverloadProd
Maybe<ovrld:BinaryProd> ::= h::Type r::Type env::Decorated Env
{
  overloads <-
    [pair(
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
      ),
      addTensor(_, _, location=_)
    )];

  lOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      addTensor(_, _, location=_)
    )];
  
  rOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      addTensor(_, _, location=_)
    )];
}
