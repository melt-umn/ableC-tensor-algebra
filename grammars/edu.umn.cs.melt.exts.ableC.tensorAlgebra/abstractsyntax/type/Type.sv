grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:type;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;
import edu:umn:cs:melt:ableC:abstractsyntax:overloadable;

abstract production tensorTypeExpr
top::BaseTypeExpr ::= q::Qualifiers fmt::Name
{

  top.pp = pp"${terminate(space(), q.pps)}tensor<${text(fmt.name)}>";
  local localErrors::[Message] =
    checkTensorHeader(top.env) ++ fmt.tensorFormatLookupCheck;
  
  propagate env;

  forwards to
    if !null(localErrors)
    then errorTypeExpr(localErrors)
    else extTypeExpr(q, tensorType(fmt));
}

abstract production tensorType
top::ExtType ::= fmt::Decorated Name
{
  propagate canonicalType;

  top.pp = pp"tensor<${text(fmt.name)}>";
  
  local fmtNm::String = fmt.tensorFormat.proceduralName;
  top.host =
    extType(
      top.givenQualifiers,
      refIdExtType(
        structSEU(),
        just(s"tensor_${fmtNm}"),
        s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"));
  top.mangledName = s"tensor_${fmt.name}";
  top.isEqualTo =
    \ other::ExtType ->
      case other of
      | tensorType(otherFmt) -> fmt.name == otherFmt.name
      | _ -> false
      end;
  
  top.memberProd = just(accessMember(_, _, _)); 
  top.arraySubscriptProd = just(accessTensor(_, _));
  top.eqArraySubscriptProd = just(accessTensorAssign(_, _, _));
  top.lEqProd = just(tensorDeepCopy(_, _));
  top.rEqProd = just(tensorDeepCopy(_, _));
}

abstract production tensorAccType
top::ExtType ::= 
{
  propagate canonicalType;

  top.pp = pp"tensor_acc";
  
  top.host =
    extType(
      top.givenQualifiers,
      refIdExtType(
        structSEU(),
        just("tensor_acc"),
        s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_acc"));
  top.mangledName = s"tensor_acc";
  top.isEqualTo =
    \ other::ExtType ->
      case other of
      | tensorAccType() -> true
      | _ -> false
      end;
  
  top.lAddProd = just(addTensor(_, _));
  top.rAddProd = just(addTensor(_, _));
  top.lSubProd = just(subTensor(_, _));
  top.rSubProd = just(subTensor(_, _));
  top.lMulProd = just(mulTensor(_, _));
  top.rMulProd = just(mulTensor(_, _));
  top.lDivProd = just(divTensor(_, _));
  top.rDivProd = just(divTensor(_, _));
  top.rEqProd = just(tensorAssignToScalar(_, _));
  top.lEqProd = just(tensorAssignToScalar(_, _));
}
