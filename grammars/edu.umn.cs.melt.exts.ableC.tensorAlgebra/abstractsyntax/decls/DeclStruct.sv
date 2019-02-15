grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declStruct
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    maybeTagDecl(
      s"tensor_${fmtNm}",
      ableC_Decl {
        struct __attribute__((refId($stringLiteralExpr{s"edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_${fmtNm}"}))) $name{s"tensor_${fmtNm}"} {
          unsigned long* dims;
          unsigned long*** indices;
          double* data;

          unsigned long bufferCnt;
          struct __tensor_tree* buffer;
          char* form;
          unsigned long dataLen;

          pthread_rwlock_t lock;
        };
      }
    );
}
