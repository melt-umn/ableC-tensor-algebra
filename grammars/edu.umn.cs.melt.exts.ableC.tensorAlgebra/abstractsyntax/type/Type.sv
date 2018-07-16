grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:type;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorTypeExpr
top::BaseTypeExpr ::= q::Qualifiers fmt::Name
{
  forwards to
    directTypeExpr(tensorType(q, fmt, top.env));
}

abstract production tensorType
top::Type ::= q::Qualifiers fmt::Name env::Decorated Env
{
  top.lpp = pp"tensor<${text(fmt.name)}>";
  top.rpp = pp"";
  
  fmt.env = env;
  
  local fmtNm::String =
    fmt.tensorFormat.proceduralName;
  
  local lErrors::[Message] =
    fmt.tensorFormatLookupCheck;
  
  forwards to
    if !null(lErrors)
    then errorType()
    else
      tagType(
        q,
        refIdTagType(
          structSEU(),
          s"tensor_${fmtNm}",
          s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"
        )
      );
}
