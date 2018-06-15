grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFilledFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return decls(consDecl(
    declMakeFunction(fmt),
    consDecl(
      declPackFunction(fmt),
      consDecl(
        maybeValueDecl(
          s"tensor_makeFilled_${fmtNm}",
          parseDecl(generateMakeFilledFunction(fmt))
        ),
        nilDecl()
      )
    )
  ));
}

function generateMakeFilledFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static struct tensor_${fmtNm}* tensor_makeFilled_${fmtNm}(unsigned long* dims, double* data) {
      struct tensor_${fmtNm}* res = tensor_make_${fmtNm}(dims);
      unsigned long index_0 = 0;
      unsigned long size = ${generateProductDims(fmt.dimens, 0)};
      res->buffer = GC_malloc(sizeof(struct tensor_insertion_s) * size);
      res->bufferLen = size;
      res->bufferCnt = size;
      ${generateMakeFilledBody(fmt.dimens, 0)}
      tensor_pack_${fmtNm}(res);
      return res;
    }
  """;
}

function generateMakeFilledBody
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  local next::String = toString(idx + 1);
  
  return
    if idx == dims
    then s"""
      unsigned long* index = GC_malloc(sizeof(unsigned long) * ${toString(dims)});
      ${generateInitiateIndex(dims, 0)}
      res->buffer[index_${index}].val = data[index_${index}];
      res->buffer[index_${index}].index = index;
    """
    else s"""
      unsigned long end_${index} = dims[${index}];
      for(unsigned long a_${index} = 0; a_${index} < end_${index}; a_${index}++) {
        unsigned long index_${next} = index_${index} * end_${index} + a_${index};
        ${generateMakeFilledBody(dims, idx+1)}
      }
    """;
}

function generateInitiateIndex
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if idx + 1 == dims
    then s"index[${index}] = a_${index};"
    else s"""
      index[${index}] = a_${index};
      ${generateInitiateIndex(dims, idx+1)}
    """;
}
