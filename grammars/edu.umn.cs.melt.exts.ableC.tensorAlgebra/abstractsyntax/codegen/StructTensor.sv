grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declStruct
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return maybeTagDecl(
           s"tensor_${fmtNm}",
           parseDecl(s"""
             struct __attribute__((refId("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"))) tensor_${fmtNm} {
               unsigned long* dims;
               unsigned long*** indices;
               double* data;
               
               struct tensor_insertion_s* buffer;
               unsigned long bufferLen;
               unsigned long bufferCnt;
             };
           """)
         );
}
