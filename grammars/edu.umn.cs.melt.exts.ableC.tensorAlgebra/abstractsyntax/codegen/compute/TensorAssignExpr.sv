grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensorAssign :: TensorExpr;
synthesized attribute tensorValue :: TensorExpr;
synthesized attribute tensorFormat :: tm:Map<Name TensorFormatItem>;
nonterminal TensorAssignExpr with tensorAssign, tensorValue, tensorFormat, location;

abstract production assignExpr
top::TensorAssignExpr ::= base::Name acc::[String] expr::TensorExpr tensors::[Name] formats::[TensorFormatItem]
{
  top.tensorAssign = access(base, acc, location=top.location);
  top.tensorValue = expr;
  top.tensorFormat = tm:add(zipWith(pair(_, _), tensors, formats), tm:empty(\n::Name nm::Name -> compareString(n.name, nm.name)));
}

abstract production assignExprExpr
top::TensorAssignExpr ::= assign::TensorExpr expr::TensorExpr fmt::tm:Map<Name TensorFormatItem>
{
  top.tensorAssign = assign;
  top.tensorValue = expr;
  top.tensorFormat = fmt;
}

abstract production nullAssignExpr
top::TensorAssignExpr ::=
{
  top.tensorAssign = nullTensorExpr(location=top.location);
  top.tensorValue = nullTensorExpr(location=top.location);
  top.tensorFormat = tm:empty(\n::Name nm::Name -> compareString(n.name, nm.name));
}
