grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:ovrld;

imports edu:umn:cs:melt:exts:ableC:tensorAlgebra;

aspect function ovrld:getArraySubscriptOverloadProd
Maybe<(Expr ::= Expr Expr Location)> ::= t::Type env::Decorated Env
{
  overloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor",
      accessTensor(_, _, env, location=_)
    )];
}

aspect function ovrld:getSubscriptAssignOverloadProd
Maybe<(Expr ::= Expr Expr (Expr ::= Expr Expr Loc) Expr Location)> ::= t::Type env::Decorated Env
{
  overloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor",
      accessTensorAssign(_, _, _, _, env, location=_)
    )];
}

aspect function ovrld:getEqOverloadProd
Maybe<ovrld:BinaryProd> ::= l::Type r::Type env::Decorated Env
{
  overloads <-
    pair(
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
      ),
      \ l::Expr r::Expr loc::Location ->
        errorExpr([err(loc, "This should not occur")], location=loc)
    ) -- This never occurs, is performed by subscript assign
    ::
    pair( -- We overload for tensor to ensure a deep copy
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor"
      ),
      \ l::Expr r::Expr loc::Location ->
        tensorDeepCopy(l, r, location=loc)
    )
    :: [];

  lOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      \ l::Expr r::Expr loc::Location ->
        errorExpr([err(loc, "This should not occur")], location=loc)
    )]; -- A tensor_acc on the lhs is a subscript assign

  rOverloads <-
    [pair( -- e.g. s = A[i, j];
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      tensorAssignToScalar(_, _, location=_)
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

aspect function ovrld:getSubOverloadProd
Maybe<ovrld:BinaryProd> ::= h::Type r::Type env::Decorated Env
{
  overloads <-
    [pair(
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
      ),
      subTensor(_, _, location=_)
    )];

  lOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      subTensor(_, _, location=_)
    )];
  
  rOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      subTensor(_, _, location=_)
    )];
}

aspect function ovrld:getMulOverloadProd
Maybe<ovrld:BinaryProd> ::= h::Type r::Type env::Decorated Env
{
  overloads <-
    [pair(
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
      ),
      mulTensor(_, _, location=_)
    )];

  lOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      mulTensor(_, _, location=_)
    )];
  
  rOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      mulTensor(_, _, location=_)
    )];
}

aspect function ovrld:getDivOverloadProd
Maybe<ovrld:BinaryProd> ::= h::Type r::Type env::Decorated Env
{
  overloads <-
    [pair(
      pair(
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
        "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"
      ),
      divTensor(_, _, location=_)
    )];

  lOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      divTensor(_, _, location=_)
    )];
  
  rOverloads <-
    [pair(
      "edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc",
      divTensor(_, _, location=_)
    )];
}
