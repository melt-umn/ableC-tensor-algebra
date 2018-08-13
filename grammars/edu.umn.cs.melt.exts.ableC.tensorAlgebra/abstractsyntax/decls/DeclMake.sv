grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;

  return
    maybeValueDecl(
      s"tensor_make_${fmtNm}",
      declTensorMake(fmt)
    );
}

function declTensorMake
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return 
    parseDecl(s"""
      static void tensor_make_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* dims) {
        t->dims = calloc(${toString(dimens)}, sizeof(unsigned long));
        memcpy(t->dims, dims, sizeof(unsigned long) * ${toString(dimens)});
        
        t->bufferCnt = 0;
        t->data = 0;
        t->indices = calloc(${toString(dimens)}, sizeof(unsigned long**));
        unsigned long count =  1;
        
        ${generateMakeBody(fmt.storage)}
        
        t->bufferCnt = 0;
        t->buffer.isLeaf = 0;
        t->buffer.index = 0;
        t->buffer.numChildren = 0;
        t->buffer.children = 0;
        
        t->form = "";
        t->dataLen = count;
      }
    """);
}

function generateMakeBody
String ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  return
    if null(storage)
    then "t->data = calloc(count, sizeof(double));"
    else 
      let dim::Integer = head(storage).snd.fst in
      let dimen::String = toString(dim) in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then s"""
        t->indices[${dimen}] = calloc(1, sizeof(unsigned long*));
        t->indices[${dimen}][0] = &(t->dims[${dimen}]);
        count *= t->dims[${dimen}];
        ${generateMakeBody(tail(storage))}
      """
      else s"""
        t->indices[${dimen}] = calloc(2, sizeof(unsigned long*));
        t->indices[${dimen}][0] = calloc(count + 1, sizeof(unsigned long));
        count = 1;
        ${generateMakeBody(tail(storage))}
      """
      end
      end
      end;
}
