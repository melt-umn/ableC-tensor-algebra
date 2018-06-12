grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declTensorGet
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_get_${fmtNm}",
    parseDecl(generateGetFunction(fmt)));
}

function generateGetFunction
String ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimens;
  
  return s"""
    static double tensor_get_${fmtNm}(struct tensor_s* t, unsigned long* index) {
      unsigned long* dims = t->dims;
      unsigned long*** indices = t->indices;
      double* data = t->data;
      unsigned long pTI = 0;
      char found = 0;
      
      unsigned long start, end;
      
      ${generateIndexCheck(dimens)}
      ${generateGetBody(fmt.order, fmt.types)}
    }
  """;
}

function generateGetBody
String ::= order::[Integer] specs::[Integer]
{
  return
    if !null(order)
    then let dim::Integer = head(order) in
         let dimen::String = toString(dim) in
         let spec::Integer = getElem(specs, dim) in
         if spec == storeDense
         then s"""
           pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
           ${generateGetBody(tail(order), specs)}
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
             return 0.0;
           }
           ${generateGetBody(tail(order), specs)}
         """
         end end end
    else s"return data[pTI];";
}
