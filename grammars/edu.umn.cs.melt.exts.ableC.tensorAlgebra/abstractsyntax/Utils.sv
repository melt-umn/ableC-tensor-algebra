imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports silver:langutil;

function checkTensorHeader
[Message] ::= loc::Location env::Decorated Env
{
  return
    if !null(lookupValue("edu_umn_cs_melt_exts_ableC_tensorAlgebra", env))
    then []
    else [err(loc, "Missing include of tensors.xh")];
}
