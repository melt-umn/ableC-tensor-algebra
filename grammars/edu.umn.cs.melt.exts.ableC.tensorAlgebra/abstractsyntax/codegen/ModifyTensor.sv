grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declModifyFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_modify_${fmtNm}",
    parseDecl(generateModifyFunction(fmt))
  );
}

function generateModifyFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimens;
  
  return s"""
    static void tensor_modify_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index, double val) {
      unsigned long* dims = t->dims;
      unsigned long*** indices = t->indices;
      double* data = t->data;
      unsigned long pTI = 0;
      char found = 0;
      unsigned long start, end;
      
      ${generateIndexCheck(dimens)}
      ${generateModifyBody(fmt.dimenOrder, fmt.specifiers, fmtNm, dimens)}
    }
  """;
}

function generateModifyBody
String ::= order::[Integer] types::[Integer] fmtNm::String dims::Integer
{
  return
    if !null(order)
    then let dim::Integer = head(order) in
         let dimen::String = toString(dim) in
         let spec::Integer = getElem(types, dim) in
         if spec == storeDense
         then s"""
           pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
           ${generateModifyBody(tail(order), types, fmtNm, dims)}
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
             if(val != 0.0 || t->bufferCnt > 0) {
               tensor_insertBuff_${fmtNm}(&(t->buffer), index, val);
               t->bufferCnt += 1;
             }
             return;
           }
           ${generateModifyBody(tail(order), types, fmtNm, dims)}
         """
         end end end
    else s"data[pTI] = val;";
}
