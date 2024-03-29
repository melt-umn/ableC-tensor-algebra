grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Production for declaring a tensor format. Checks that the format
   being created is valid, and not a redeclaration (preventing a 
   single name being declared twice. Internally we could allow this,
   but then the type becomes dependent on the scope. -}
abstract production format
top::Decl ::= nm::Name specs::[Integer] order::[Integer]
{
  propagate controlStmtContext, env;

  local errors::[Message] =
    checkTensorHeader(top.env)
    ++
    (
    if listLength(specs) == 0
    then [errFromOrigin(nm, "Zero dimensional tensors (scalars) cannot be declared as tensor formats. Declare values as normal double values.")]
    else []
    )
    ++
    (
    if listLength(specs) != listLength(order)
    then [errFromOrigin(nm, "Tensor format must have same number of dimensions in specifiers and order.")]
    else []
    )
    ++
    (
    if containsAll(makeList(inc, 0, listLength(order)), order)
    then []
    else [errFromOrigin(nm, s"Tensor format's order must contain each dimension once, represented as an integer 0, 1, ..., ${toString(listLength(specs)-1)}.")]
    )
    ++
    nm.tensorFormatRedeclarationCheck;
  
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

  local fmt::TensorFormat = tensorFormat(specs, order);
  
  local fwrd::Decl =
    decls(consDecl(
      declAll(fmt), -- Add all tensor methods for accessing, packing, etc.
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
