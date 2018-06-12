grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:type;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports silver:langutil:pp;

abstract production formatTypeExpr
top::BaseTypeExpr ::= q::Qualifiers
{
  forwards to typeModifierTypeExpr(
                directTypeExpr(formatType(q)),
                pointerTypeExpr(nilQualifier(), baseTypeExpr()));
}

abstract production formatType
top::Type ::= q::Qualifiers
{
  top.lpp = pp"tensor format";
  top.rpp = pp"";
  
  forwards to
    tagType(
      q,
      refIdTagType(structSEU(), "tensor_format_s", s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:format"));
}

abstract production tensorTypeExpr
top::BaseTypeExpr ::= q::Qualifiers
{
  forwards to typeModifierTypeExpr(
                directTypeExpr(tensorType(q)),
                pointerTypeExpr(nilQualifier(), baseTypeExpr()));
}

abstract production tensorType
top::Type ::= q::Qualifiers
{
  top.lpp = pp"tensor";
  top.rpp = pp"";
  
  forwards to
    tagType(
      q,
      refIdTagType(structSEU(), "tensor_s", s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor"));
}
