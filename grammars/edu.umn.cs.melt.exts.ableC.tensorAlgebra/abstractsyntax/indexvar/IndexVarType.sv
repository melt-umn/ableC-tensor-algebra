grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:indexvar;

abstract production indexVarType
t::Type ::= loc::Location
{
  forwards to
    attributedType(
      consAttribute(
        gccAttribute(
          consAttrib(
            appliedAttrib(
              attribName(name("module", location=loc)),
              consExpr(
                stringLiteral("edu:umn:cs:melt:exts:ableC:tensorAlgebra:indexvar", location=loc),
                nilExpr()
              )
            ),
            nilAttrib()
          )
        ), 
        nilAttribute()
      ), 
      builtinType(
        nilQualifier(), 
        signedType(intType())
      )
    );
}
