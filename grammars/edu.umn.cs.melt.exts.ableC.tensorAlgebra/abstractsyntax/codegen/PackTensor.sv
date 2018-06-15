grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declPackFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return decls(consDecl(
    declTensorGet(fmt),
    consDecl(
      maybeValueDecl(
        s"tensor_generateIndex_${fmtNm}",
        parseDecl(generateGenerateIndexFunction(fmt))
      ), consDecl(
        maybeValueDecl(
          s"tensor_getBuff_${fmtNm}",
          parseDecl(generateGetBufferFunction(fmt))
        ), consDecl(
          maybeValueDecl(
            s"tensor_pack_${fmtNm}",
            parseDecl(generatePackFunction(fmt))
          ), nilDecl()
        )
      )
    )
  ));
}


function generateGenerateIndexFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  local order::String = toString(fmt.dimens);
  local ordered::[Integer] = reverse(fmt.dimenOrder);
  
  return s"""
    static unsigned long* tensor_generateIndex_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long higher, unsigned long bottom) {
      unsigned long* dims = t->dims;
      unsigned long* res = GC_malloc(sizeof(unsigned long) * ${order});
      res[${toString(head(ordered))}] = bottom;
      ${generateGenerateIndexBody(tail(ordered))}
      return res;
    }
  """;
}

function generateGenerateIndexBody
String ::= order::[Integer]
{
  local dim::String = toString(head(order));
  
  return 
    if !null(order)
    then s"""
      res[${dim}] = (higher % dims[${dim}]);
      higher = higher / dims[${dim}];
      ${generateGenerateIndexBody(tail(order))}
    """
    else s"";
}


function generateGetBufferFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static double tensor_getBuff_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index) {
      struct tensor_insertion_s* buffer = t->buffer;
      unsigned long bufferCnt = t->bufferCnt;
      double res = tensor_get_${fmtNm}(t, index);
      for(unsigned long i = 0; i < bufferCnt; i++) {
        char match = 1;
        if(0) {}
        ${generateGetBufferBody(fmt.dimens, 0)}
      }
      
      return res;
    }
  """;
}

function generateGetBufferBody
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if dims == idx
    then s"""
      else {
        res = buffer[i].val;
      }
    """
    else s"""
      else if(index[${index}] != buffer[i].index[${index}]) {
      
      }
      ${generateGetBufferBody(dims, idx + 1)}
    """;
}


function generatePackFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  local order::Integer = fmt.dimens;
  local dimOrder::[Integer] = reverse(fmt.dimenOrder);
  local dimType::[Integer] = fmt.specifiers;
  local firstDim::Integer = head(dimOrder);
  
  return s"""
    static void tensor_pack_${fmtNm}(struct tensor_${fmtNm}* t) {
      unsigned long* dims = t->dims;
      unsigned long*** indices = t->indices;
      double* data = t->data;
      
      if(t->bufferCnt > 0) {
        unsigned long size = ${generateProductDims(order, 0)};
        
        unsigned long dimSize = dims[${toString(firstDim)}];
        size /= dimSize;
        struct tensor_tree_s* tree = GC_malloc(sizeof(struct tensor_tree_s) * size);
        
        for(unsigned long k = 0; k < size; k++) {
          ${generatePackBody_Leaf(firstDim, getElem(dimType, firstDim), fmtNm)}
        }
        
        struct tensor_tree_s* temp;
        ${generatePackBody_Tree(tail(dimOrder), dimType)}
        
        t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(order)});
        unsigned long numChildren = 1;
        struct tensor_tree_s** trees = &tree;
        
        unsigned long newChildren;
        unsigned long index;
        struct tensor_tree_s** temp_tree;
        unsigned long total;
        
        ${generatePackBody_Assemble(fmt.dimenOrder, dimType)}
        
        t->data = GC_malloc(sizeof(double) * numChildren);
        for(unsigned long i = 0; i < numChildren; i++) {
          t->data[i] = trees[i]->val;
        }
        t->bufferCnt = 0;
      }
    }
  """;
}

function generatePackBody_Leaf
String ::= dim::Integer type::Integer fmtNm::String
{
  return if type == storeDense
         then s"""
           tree[k].numChildren = dimSize;
           tree[k].children = GC_malloc(sizeof(struct tensor_tree_s) * dimSize);
           for(unsigned long j = 0; j < dimSize; j++) {
             tree[k].children[j].isLeaf = 1;
             tree[k].children[j].val = tensor_getBuff_${fmtNm}(t, tensor_generateIndex_${fmtNm}(t, k, j));
             tree[k].children[j].index = j;
           }
         """
         else s"""
           unsigned int count = 0;
           struct tensor_tree_s* temp = GC_malloc(sizeof(struct tensor_tree_s) * dimSize);
           for(unsigned long j = 0; j < dimSize; j++) {
             unsigned long* index = tensor_generateIndex_${fmtNm}(t, k, j);
             double val = tensor_getBuff_${fmtNm}(t, index);
             if(val != 0.0) {
               temp[count].val = val;
               temp[count].index = j;
               count++;
             }
           }
           
           tree[k].numChildren = count;
           tree[k].children = GC_malloc(sizeof(struct tensor_tree_s) * count);
           for(unsigned long j = 0; j < count; j++) {
             tree[k].children[j].isLeaf = 1;
             tree[k].children[j].val = temp[j].val;
             tree[k].children[j].index = temp[j].index;
           }
         """;
}

function generatePackBody_Tree
String ::= dims::[Integer] specs::[Integer]
{
  local dimen::Integer = head(dims);
  local dim::String = toString(dimen);
  local spec::Integer = getElem(specs, dimen);

  return if null(dims)
         then s""
         else s"""
           dimSize = dims[${dim}];
           size /= dimSize;
           temp = GC_malloc(sizeof(struct tensor_tree_s) * size);
           for(unsigned long k = 0; k < size; k++) {
             ${if spec == storeDense
               then s"""
                 temp[k].numChildren = dimSize;
                 temp[k].children = GC_malloc(sizeof(struct tensor_tree_s) * dimSize);
                 for(unsigned long j = 0; j < dimSize; j++) {
                   temp[k].children[j].isLeaf = 0;
                   temp[k].children[j].index = j;
                   temp[k].children[j].numChildren = tree[k * dimSize + j].numChildren;
                   temp[k].children[j].children = tree[k * dimSize + j].children;
                 }
               """
               else s"""
                 unsigned long count = 0;
                 struct tensor_tree_s* tmp = GC_malloc(sizeof(struct tensor_tree_s) * dimSize);
                 for(unsigned long j = 0; j < dimSize; j++) {
                   if(tensor_checkTree(&(tree[k * dimSize + j]))) {
                     tmp[count].numChildren = tree[k * dimSize + j].numChildren;
                     tmp[count].children = tree[k * dimSize + j].children;
                     tmp[count].index = j;
                     count++;
                   }
                 }
                 
                 temp[k].index = k;
                 temp[k].numChildren = count;
                 temp[k].children = GC_malloc(sizeof(struct tensor_tree_s) * count);
                 for(unsigned long j = 0; j < count; j++) {
                   temp[k].children[j].isLeaf = 0;
                   temp[k].children[j].index = tmp[j].index;
                   temp[k].children[j].numChildren = tmp[j].numChildren;
                   temp[k].children[j].children = tmp[j].children;
                 }
               """
             }
           }
           tree = temp;
           
           ${generatePackBody_Tree(tail(dims), specs)}
         """;
}

function generatePackBody_Assemble
String ::= dims::[Integer] specs::[Integer]
{
  local dimInt::Integer = head(dims);
  local dim::String = toString(dimInt);
  local type::Integer = getElem(specs, dimInt);
  
  return if null(dims)
         then s""
         else s"""
           dimSize = dims[${dim}];
           newChildren = 0;
           for(unsigned long j = 0; j < numChildren; j++) {
             newChildren += trees[j]->numChildren;
           }
           temp_tree = GC_malloc(sizeof(struct tensor_tree_s*) * newChildren);
           index = 0;
           for(unsigned long j = 0; j < numChildren; j++) {
             unsigned long end = trees[j]->numChildren;
             for(unsigned long k = 0; k < end; k++) {
               temp_tree[index] = &(trees[j]->children[k]);
               index++;
             }
           }
           
           ${if type == storeDense
             then s"""
               t->indices[${dim}] = GC_malloc(sizeof(unsigned long*));
               t->indices[${dim}][0] = GC_malloc(sizeof(unsigned long));
               t->indices[${dim}][0][0] = dimSize;
             """ 
             else s"""
               t->indices[${dim}] = GC_malloc(sizeof(unsigned long*) * 2);
               t->indices[${dim}][0] = GC_malloc(sizeof(unsigned long) * (numChildren + 1));
               t->indices[${dim}][1] = GC_malloc(sizeof(unsigned long) * newChildren);
               
               for(unsigned long k = 0; k < newChildren; k++) {
                 t->indices[${dim}][1][k] = temp_tree[k]->index;
               }
               total = 0;
               t->indices[${dim}][0][0] = total;
               for(unsigned long k = 0; k < numChildren; k++) {
                 total += trees[k]->numChildren;
                 t->indices[${dim}][0][k+1] = total;
               }
             """
           }           
           numChildren = newChildren;
           trees = temp_tree;
           ${generatePackBody_Assemble(tail(dims), specs)}
         """;
}
