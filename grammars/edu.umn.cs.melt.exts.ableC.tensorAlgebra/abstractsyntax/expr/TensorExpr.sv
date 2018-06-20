grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute errors::[Message];
nonterminal TensorExpr with pp, errors, env, location;

abstract production nullTensorExpr
top::TensorExpr ::=
{
  top.pp = ppConcat([text("null")]);
  top.errors = [];
}

abstract production access
top::TensorExpr ::= name::Name access::[String]
{
  top.pp = ppConcat([
      text(name.name),
      text("("),
      ppImplode(
        text(", "),
        map(text(_), access)
      ),
      text(")")
    ]);

  top.errors =
    case lookupValue(name.name, top.env) of
    | b::[] -> case b.typerep of
               | pointerType(_,
                   tensorType(_, _, _)
                 ) -> []
               | _ -> [err(top.location, s"Tensor access expected a tensor (got ${showType(b.typerep)}")]
               end
    | _ -> [err(top.location, s"Tesnsor access expcted a tensor")]
    end;
}

abstract production tExpr
top::TensorExpr ::= expr::Expr
{
  top.pp = expr.pp;
  expr.returnType = nothing();
  
  top.errors =
    case expr of
    | errorExpr(errs) -> errs
    | _ -> if expr.typerep.isArithmeticType
           then []
           else [err(top.location, s"Expected numeric expression (got ${showType(expr.typerep)}")]
    end;
}

abstract production add
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" + "),
    right.pp,
    text(")")
  ]);
  
  top.errors = left.errors ++ right.errors;
}

abstract production mul
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" * "),
    right.pp,
    text(")")
  ]);

  top.errors = left.errors ++ right.errors;
}
