grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declAccessFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_access_${fmtNm}",
    parseDecl(generateAccessFunction(fmt))
  );
}

function generateAccessFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimens;
  
  return s"""
    static double* tensor_access_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index) {
      unsigned long* dims = t->dms;
      unsigned long*** indices = t->indices;
      double* data = t->data;
      unsigned long pTI = 0;
      char found = 0;
      unsigned long start, end;
      
      ${generateIndexCheck(dimens)}
      ${generateAccessBody(fmt.dimenOrder, fmt.specifiers, fmtNm, dimens)}
    }
  """;
}

function generateAccessBody
String ::= order::[Integer] types::[Integer] fmtNm::String dims::Integer
{
  return
    if !null(order)
    then 
      let dim::Integer = head(order) in
      let dimen::String = toString(dim) in
      let spec::Integer = case getElem(types, dim) of
                          | nothing() -> 0
                          | just(i) -> i
                          end
      in
      if spec == storeDense
      then s"""
        pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
        ${generateAccessBody(tail(order), types, fmtNm, dims)}
      """
      else s"""
        found = 0;
        start = indices[${dimen}][0][pTI];
        end = indices[${dimen}][0][pTI + 1];
        for(unsigned long j = start; j < end; j++) {
          if(indices[${dimen}][1][j] == index[${dimen}]) {
            pTI = j;
            found = 1;
            break;
          }
        }
        if(!found) {
          t->bufferCnt += 1;
          return tensor_insertBuff_zero_${fmtNm}(&(t->buffer), index);
        }
        ${generateAccessBody(tail(order), types, fmtNm, dims)}
      """
      end
      end
      end
    else s"return data + pTI;";
}
