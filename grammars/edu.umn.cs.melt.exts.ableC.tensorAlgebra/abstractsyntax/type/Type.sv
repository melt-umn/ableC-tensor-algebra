grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:type;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;
import edu:umn:cs:melt:ableC:abstractsyntax:overloadable;

abstract production tensorTypeExpr
top::BaseTypeExpr ::= q::Qualifiers fmt::Name
{
  propagate substituted;

  top.pp = pp"${terminate(space(), q.pps)}tensor<${text(fmt.name)}>";
  local localErrors::[Message] =
    checkTensorHeader(fmt.location, top.env) ++ fmt.tensorFormatLookupCheck;
  
  forwards to
    if !null(localErrors)
    then errorTypeExpr(localErrors)
    else extTypeExpr(q, tensorType(fmt));
}

abstract production tensorType
top::ExtType ::= fmt::Decorated Name
{
  propagate canonicalType, substituted;

  top.pp = pp"tensor<${text(fmt.name)}>";
  
  local fmtNm::String = fmt.tensorFormat.proceduralName;
  top.host =
    extType(
      top.givenQualifiers,
      refIdExtType(
        structSEU(),
        s"tensor_${fmtNm}",
        s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"));
  top.mangledName = s"tensor_${fmt.name}";
  top.isEqualTo =
    \ other::ExtType ->
      case other of
      | tensorType(otherFmt) -> fmt.name == otherFmt.name
      | _ -> false
      end;
  
  top.memberProd = just(accessMember(_, _, _, location=_)); 
  top.arraySubscriptProd = just(accessTensor(_, _, location=_));
  top.eqArraySubscriptProd = just(accessTensorAssign(_, _, _, location=_));
  top.lEqProd = just(tensorDeepCopy(_, _, location=_));
  top.rEqProd = just(tensorDeepCopy(_, _, location=_));
}

abstract production tensorAccType
top::ExtType ::= 
{
  propagate canonicalType, substituted;

  top.pp = pp"tensor_acc";
  
  top.host =
    extType(
      top.givenQualifiers,
      refIdExtType(
        structSEU(),
        s"tensor_acc",
        s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"));
  top.mangledName = s"tensor_acc";
  top.isEqualTo =
    \ other::ExtType ->
      case other of
      | tensorAccType() -> true
      | _ -> false
      end;
  
  top.lAddProd = just(addTensor(_, _, location=_));
  top.rAddProd = just(addTensor(_, _, location=_));
  top.lSubProd = just(subTensor(_, _, location=_));
  top.rSubProd = just(subTensor(_, _, location=_));
  top.lMulProd = just(mulTensor(_, _, location=_));
  top.rMulProd = just(mulTensor(_, _, location=_));
  top.lDivProd = just(divTensor(_, _, location=_));
  top.rDivProd = just(divTensor(_, _, location=_));
  top.rEqProd = just(tensorAssignToScalar(_, _, location=_));
  top.lEqProd = just(tensorAssignToScalar(_, _, location=_));
}
