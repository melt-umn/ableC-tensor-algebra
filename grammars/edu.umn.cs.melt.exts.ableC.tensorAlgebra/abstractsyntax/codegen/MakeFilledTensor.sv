grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFilledFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_makeFilled_${fmtNm}",
    parseDecl(generateMakeFilledFunction(fmt))
  );
}

function generateMakeFilledFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static struct tensor_${fmtNm}* tensor_makeFilled_${fmtNm}(unsigned long* dims, double* data) {
      struct tensor_${fmtNm}* res = tensor_make_${fmtNm}(dims);

      unsigned long index = 0;
      struct tensor_tree_s* buffer = &(res->buffer);

      ${generateMakeFilledBody(fmt.dimens, 0, fmtNm)}

      res->bufferCnt = ${generateProductDims(fmt.dimens, 0)};
      return res;
    }
  """;
}

function generateMakeFilledBody
String ::= dims::Integer idx::Integer fmtNm::String
{
  local index::String = toString(idx);
  local next::String = toString(idx + 1);
  
  return
    if idx == dims
    then s"""
      unsigned long idx[] = {${generateIndexArray(dims)}};
      tensor_insertBuff_${fmtNm}(buffer, idx, data[index]);
      index++;
    """
    else s"""
      for(unsigned long i${next} = 0; i${next} < dims[${index}]; i${next}++) {
        ${generateMakeFilledBody(dims, idx+1, fmtNm)}
      }
    """;
}
