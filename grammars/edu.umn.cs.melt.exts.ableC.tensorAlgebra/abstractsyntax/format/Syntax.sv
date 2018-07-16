grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production format
top::Decl ::= nm::Name specs::[Integer] order::[Integer]
{
  local errors::[Message] =
    (
    if listLength(specs) != listLength(order)
    then [err(nm.location, "Tensor format must have same number of dimensions in specifiers and order.")]
    else []
    )
    ++
    (
    if containsAll(integerEqual, makeList(integerCompare, inc, 0, listLength(order)), order)
    then []
    else [err(nm.location, "Tensor format's order must contain each dimension once, represented as an integer 0, 1, ..., d-1.")]
    )
    ++
    nm.tensorFormatRedeclarationCheck;
  
  propagate substituted;
  top.pp = ppConcat([
             text("tensor format "),
             text(nm.name),
             text(" ({"),
             ppImplode(text(", "), map(\a::Integer -> text(if a == storeDense
                                                           then "dense"
                                                           else "sparse"), specs)),
             text("}, {"),
             ppImplode(text(", "), map(\a::Integer -> text(toString(a)), order)),
             text("});")
           ]);

  local fmt::TensorFormat = tensorFormat(specs, order, nm.location);
  
  local fwrd::Decl =
    decls(consDecl(
      declAll(fmt),
      consDecl(
        defsDecl([tensorFormatDef(nm.name, fmt)]),
        nilDecl()
      )
    ));
  
  forwards to
  if !null(errors)
  then warnDecl(errors)
  else fwrd;
}
