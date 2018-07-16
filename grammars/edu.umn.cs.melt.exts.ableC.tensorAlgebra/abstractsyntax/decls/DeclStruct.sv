grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declStruct
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    maybeTagDecl(
      s"tensor_${fmtNm}",
      parseDecl(
        if fmt.dimensions != 0
        then s"""
          struct __attribute__((refId("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"), module("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor"))) tensor_${fmtNm} {
            unsigned long* dims;
            unsigned long*** indices;
            double* data;
            
            unsigned long bufferCnt;
            struct tensor_tree_s buffer;
            char* form;
            unsigned long dataLen;
          };
        """
        else s"""
          struct __attribute__((refId("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"), module("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor"))) tensor_${fmtNm} {
            double* data;
            
            char* form;
          };          
        """
      )
    );
}
