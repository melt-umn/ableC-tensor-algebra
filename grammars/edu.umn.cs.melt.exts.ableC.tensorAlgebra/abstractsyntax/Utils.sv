imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports silver:langutil;

function checkTensorHeader
[Message] ::= env::Decorated Env
{
  return
    if !null(lookupValue("edu_umn_cs_melt_exts_ableC_tensorAlgebra", env))
    then []
    else [errFromOrigin(ambientOrigin(), "Missing include of tensors.xh")];
}
