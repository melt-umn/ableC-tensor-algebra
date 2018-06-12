grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_make_${fmtNm}",
    parseDecl(generateMakeFunction(fmt)));
}

function generateMakeFunction
String ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimens;
  
  return s"""
    static struct tensor_s* tensor_make_${fmtNm}(unsigned long* dims) {
      struct tensor_s* res = GC_malloc(sizeof(struct tensor_s));
      res->dims = GC_malloc(sizeof(unsigned long) * ${toString(dimens)});
      memcpy(res->dims, dims, sizeof(unsigned long) * ${toString(dimens)});
      res->buffer = GC_malloc(sizeof(struct tensor_insertion_s) * 8);
      res->bufferLen = 8;
      res->bufferCnt = 0;
      res->data = 0;
      res->indices = GC_malloc(sizeof(unsigned long**) * ${toString(dimens)});
      unsigned long count = 1;
      
      ${generateMakeBody(fmt.order, fmt.types)}
      
      return res;
    }
  """;
}

function generateMakeBody
String ::= order::[Integer] types::[Integer]
{
  return
    if !null(order)
    then let dim::Integer = head(order) in
         let dimen::String = toString(dim) in
         let spec::Integer = getElem(types, dim) in
         if spec == storeDense
         then s"""
           res->indices[${dimen}] = GC_malloc(sizeof(unsigned long*));
           res->indices[${dimen}][0] = &(res->dims[${dimen}]);
           count *= res->dims[${dimen}];
           ${generateMakeBody(tail(order), types)}
         """
         else s"""
           res->indices[${dimen}] = GC_malloc(sizeof(unsigned long*) * 2);
           res->indices[${dimen}][0] = GC_malloc(sizeof(unsigned long) * (count + 1));
         """ 
         end end end
    else s"res->data = GC_malloc(sizeof(double) * count);";
}
