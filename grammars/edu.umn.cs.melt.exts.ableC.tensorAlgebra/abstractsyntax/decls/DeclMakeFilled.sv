grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFilledFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    if fmt.dimensions == 0
    then decls(nilDecl())
    else 
      maybeValueDecl(
        s"tensor_makeFilled_${fmtNm}",
        declMakeFilled(fmt)
      );
}

function declMakeFilled
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return parseDecl(s"""
    static void tensor_makeFilled_${fmtNm}(struct tensor_${fmtNm}* res, unsigned long* dims, double* data) {
      tensor_make_${fmtNm}(res, dims);
      
      unsigned long index = 0;
      struct tensor_tree_s* buffer = &(res->buffer);
      
      ${generateMakeFilledBody(fmt.dimensions, 0, fmtNm)}
      
      res->bufferCnt = ${generateProductDims(fmt.dimensions, 0)};
    }
  """);
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
