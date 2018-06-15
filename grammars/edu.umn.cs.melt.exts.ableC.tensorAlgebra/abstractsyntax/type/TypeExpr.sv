grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:type;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports silver:langutil;
imports silver:langutil:pp;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorTypeExpr
top::BaseTypeExpr ::= q::Qualifiers fmt::Name
{
  forwards to typeModifierTypeExpr(
                directTypeExpr(tensorType(q, fmt, top.env)),
                pointerTypeExpr(nilQualifier(), baseTypeExpr()));
}

abstract production tensorType
top::Type ::= q::Qualifiers fmt::Name env::Decorated Env
{
  top.lpp = pp"tensor<${text(fmt.name)}>";
  top.rpp = pp"";

  fmt.env = env;

  local fmtNm::String =
    fmt.tensorFormatItem.proceduralName;
  
  local errors::[Message] =
    fmt.tensorFormatLookupCheck;
  
  forwards to
  if !null(errors)
  then errorType()
  else tagType(
         q,
         refIdTagType(
           structSEU(), 
           s"tensor_${fmtNm}", 
           s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"
         )
       );
}
