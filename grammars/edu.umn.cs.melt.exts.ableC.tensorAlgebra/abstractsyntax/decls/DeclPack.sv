grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

imports edu:umn:cs:melt:exts:ableC:tensorAlgebra;

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
        temp = calloc(count * dims[${toString(dim)}], sizeof(struct tensor_tree_s));
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
              idx++;
            } else {
              temp[index].index = j;
              temp[index].isLeaf = 1;
              temp[index].val = 0.0;
            }
            index++;
          }
          free(oldChildren);
        }
      """
      else s"""
      
      """
    else 
      if type == storeDense
      then s"""
        index = 0;
        temp = calloc(count * dims[${toString(dim)}], sizeof(struct tensor_tree_s));
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
          free(oldChildren);
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
        
        temp = calloc(cTemp, sizeof(struct tensor_tree_s));
        index = 0;
        
        for(unsigned long i = 0; i < count; i++) {
          unsigned long idx = index;
          for(unsigned long j = 0; j < tree[i].numChildren; j++) {
            temp[index] = tree[i].children[j];
            index++;
          }
          if(tree[i].children) free(tree[i].children);
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
      if(t->bufferCnt == 0) return;

      pthread_rwlock_wrlock(&(t->lock));

      if(t->bufferCnt > 0) {
        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        struct tensor_tree_s* buffer = &(t->buffer);
        
        unsigned long pI0 = 0;
        ${generatePackBody_Tree(fmt.storage, fmtNm, fmt.dimensions, 1)}
        tensor_packTree_${fmtNm}(buffer, dims);
        
        if(t->indices) { ${freeIndices_String(fmt)} }
        t->indices = calloc(${toString(order)}, sizeof(unsigned long**));
        
        unsigned long numChildren = 1;
        struct tensor_tree_s** trees = &buffer;
        
        struct tensor_tree_s** temp_tree;
        unsigned long total, dimSize, index, newChildren;
        
        ${generatePackBody_Assemble(fmt.storage)}
        
        if(t->data) free(t->data);
        t->data = calloc(numChildren, sizeof(double));
        
        for(unsigned long i = 0; i < numChildren; i++) {
          t->data[i] = trees[i]->val;
        }
        if(trees != &buffer) {
          free(trees);
        }

        t->dataLen = numChildren;
        t->bufferCnt = 0;
        t->buffer.numChildren = 0;
        t->buffer.children = 0;
        t->form = "";
      }

      pthread_rwlock_unlock(&(t->lock));
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
      temp_tree = calloc(newChildren, sizeof(struct tensor_tree_s*));
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
          t->indices[${dim}] = calloc(1, sizeof(unsigned long*));
          t->indices[${dim}][0] = calloc(1, sizeof(unsigned long));
          t->indices[${dim}][0][0] = dimSize;
        """
        else s"""
          t->indices[${dim}] = calloc(2, sizeof(unsigned long*));
          t->indices[${dim}][0] = calloc(numChildren + 1, sizeof(unsigned long));
          t->indices[${dim}][1] = calloc(newChildren, sizeof(unsigned long));
          
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
      if(trees != &buffer) {
        free(trees);
      }
      numChildren = newChildren;
      trees = temp_tree;
      ${generatePackBody_Assemble(tail(storage))}
    """;
}
