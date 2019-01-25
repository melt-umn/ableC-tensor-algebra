grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:indexvar;

abstract production indexvarTypeExpr
top::BaseTypeExpr ::= q::Qualifiers
{
  propagate substituted;

  top.pp = pp"${terminate(space(), q.pps)}indexvar";
  
  forwards to extTypeExpr(q, indexvarType());
}

abstract production indexvarType
top::ExtType ::= 
{
  propagate canonicalType, substituted;

  top.pp = pp"indexvar";

  top.host =
    extType(
      top.givenQualifiers,
      refIdExtType(
        structSEU(),
        s"tensor_indexvar",
        s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_indexvar"));
  top.mangledName = s"indexvar";
  top.isEqualTo =
    \ other::ExtType ->
      case other of
      | indexvarType() -> true
      | _ -> false
      end;
}
