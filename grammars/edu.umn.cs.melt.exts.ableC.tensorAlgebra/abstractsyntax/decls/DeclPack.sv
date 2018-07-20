grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declPackFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return 
    decls(
      consDecl(
        maybeValueDecl(
          s"tensor_packTree_${fmtNm}",
          declPackTree(fmt)
        ),
        consDecl(
          maybeValueDecl(
            s"tensor_pack_${fmtNm}",
            declPack(fmt)
          ),
          nilDecl()
        )
      )
    );
}

function declPackTree
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return parseDecl(s"""
    static void tensor_packTree_${fmtNm}(struct tensor_tree_s* tree, unsigned long* dims) {
      unsigned long count = 1;
      unsigned long cTemp, index;
      struct tensor_tree_s* temp;
      
      ${generatePackTreeBody(fmt.storage)}
    }
  """);
}

function generatePackTreeBody
String ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dim::Integer = head(storage).snd.fst;
  local type::Integer = head(storage).snd.snd;
  
  return
    if null(tail(storage))
    then 
      if type == storeDense
      then s"""
        index = 0;
        temp = GC_malloc(sizeof(struct tensor_tree_s) * count * dims[${toString(dim)}]);
        for(unsigned long i = 0; i < count; i++) {
          unsigned long oldSize = tree[i].numChildren;
          struct tensor_tree_s*  oldChildren = tree[i].children;
          tree[i].numChildren = dims[${toString(dim)}];
          tree[i].children = &(temp[index]);
          
          unsigned idx = 0;
          for(unsigned long j = 0; j < dims[${toString(dim)}]; j++) {
            if(idx < oldSize && oldChildren[idx].index == j) {
              temp[index].index = j;
              temp[index].isLeaf = 1;
              temp[index].val = oldChildren[idx].val;
            } else {
              temp[index].index = j;
              temp[index].isLeaf = 1;
              temp[index].val = 0.0;
            }
            index++;
          }
        }
      """
      else s"""
      
      """
    else 
      if type == storeDense
      then s"""
        index = 0;
        temp = GC_malloc(sizeof(struct tensor_tree_s) * count * dims[${toString(dim)}]);
        for(unsigned long i = 0; i < count; i++) {
          unsigned long oldSize = tree[i].numChildren;
          struct tensor_tree_s* oldChildren = tree[i].children;
          struct tensor_tree_s* start = &(temp[index]);
          tree[i].numChildren = dims[${toString(dim)}];
          
          unsigned long idx = 0;
          for(unsigned long j = 0; j < dims[${toString(dim)}]; j++) {
            if(idx < oldSize && oldChildren[idx].index == j) {
              temp[index].index = j;
              temp[index].isLeaf = 0;
              temp[index].numChildren = oldChildren[idx].numChildren;
              temp[index].children = oldChildren[idx].children;
              idx++;
            } else {
              temp[index].isLeaf = 0;
              temp[index].index = j;
              temp[index].numChildren = 0;
            }
            index++;
          }
          
          tree[i].children = start;
        }
        
        count = count * dims[${toString(dim)}];
        tree = temp;
        
        ${generatePackTreeBody(tail(storage))}
      """
      else s"""
        cTemp = 0;
        for(unsigned long i = 0; i < count; i++) {
          cTemp += tree[i].numChildren;
        }
        
        temp = GC_malloc(sizeof(struct tensor_tree_s) * cTemp);
        index = 0;
        
        for(unsigned long i = 0; i < count; i++) {
          unsigned long idx = index;
          for(unsigned long j = 0; j < tree[i].numChildren; j++) {
            temp[index] = tree[i].children[j];
            index++;
          }
          tree[i].children = &(temp[idx]);
        }
        
        tree = temp;
        count = cTemp;
        
        ${generatePackTreeBody(tail(storage))}
      """;
}


function declPack
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local order::Integer = fmt.dimensions;
  
  return parseDecl(s"""
    static void tensor_pack_${fmtNm}(struct tensor_${fmtNm}* t) {
      if(t->bufferCnt > 0) {
        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        struct tensor_tree_s* buffer = &(t->buffer);
        
        unsigned long pI0 = 0;
        ${generatePackBody_Tree(fmt.storage, fmtNm, fmt.dimensions, 1)}
        tensor_packTree_${fmtNm}(buffer, dims);
        
        t->indices = GC_malloc(sizeof(unsigned long**) * ${toString(order)});
        unsigned long numChildren = 1;
        struct tensor_tree_s** trees = &buffer;
        
        struct tensor_tree_s** temp_tree;
        unsigned long total, dimSize, index, newChildren;
        
        ${generatePackBody_Assemble(fmt.storage)}
        
        t->data = GC_malloc(sizeof(double) * numChildren);
        for(unsigned long i = 0; i < numChildren; i++) {
          t->data[i] = trees[i]->val;
        }
        
        t->dataLen = numChildren;
        t->bufferCnt = 0;
        t->buffer.numChildren = 0;
        t->buffer.children = 0;
        t->form = "";
      }
    }
  """);
}

function generatePackBody_Tree
String ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String order::Integer dim::Integer
{
  local dimen::Integer = head(storage).snd.fst;
  local type::Integer = head(storage).snd.snd;
  local str_dim::String = toString(dim);
  local dims::String = toString(dimen);
  
  return
    if null(storage)
    then s"""
      unsigned long index[] = {${generateIndexArray(order)}};
      double value = data[pI${toString(dim - 1)}];
      tensor_insertBuff_${fmtNm}(buffer, index, value);
    """
    else
      if type == storeDense
      then s"""
        for(unsigned long i${toString(dim)} = 0;
            i${toString(dim)} < dims[${dims}];
            i${toString(dim)}++) {
          unsigned long pI${toString(dim)} = (pI${toString(dim - 1)} * dims[${dims}]) + i${toString(dim)};
          ${generatePackBody_Tree(tail(storage), fmtNm, order, dim+1)}
        }
      """
      else s"""
        for(unsigned long pI${toString(dim)} = indices[${dims}][0][pI${toString(dim-1)}];
            pI${toString(dim)} < indices[${dims}][0][pI${toString(dim-1)} + 1];
            pI${toString(dim)}++) {
          unsigned long i${toString(dim)} = indices[${dims}][1][pI${toString(dim)}];
          ${generatePackBody_Tree(tail(storage), fmtNm, order, dim+1)}
        }
      """;
}

function generatePackBody_Assemble
String ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dimInt::Integer = head(storage).snd.fst;
  local dim::String = toString(dimInt);
  local type::Integer = head(storage).snd.snd;
  
  return
    if null(storage)
    then ""
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
      ${generatePackBody_Assemble(tail(storage))}
    """;
}
