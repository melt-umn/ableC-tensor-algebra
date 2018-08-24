grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

imports edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Here we define the functions used to pack a tensor's buffer. The
   tensor_packTree function is responsible for taking the buffer and
   properly filling dense layers. This method is used by tensor_pack
   after adding all elements in the tensor's body to the buffer. Because
   of this, it is most efficient to pack a tensor once only. If there 
   is no data in the buffer, calls to pack have no effect.
-}
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
  
  return
    ableC_Decl {
      static void $name{s"tensor_packTree_${fmtNm}"}(struct __tensor_tree* tree, unsigned long* dims) {
        $Stmt{generatePackTreeBody(fmt.storage)}
      }
    };
}

{- This generates a nested loop structure iterating over the nodes in
   the tree. If a dimension is sparse, we just pack the tree beneath 
   each node. If a dimension is dense, we require that every possible
   node exist (a node for each index) as this is the definition of
   dense. In that case, we add any necessary nodes and pack each
   sub-tree.
-}
function generatePackTreeBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dim::Integer = head(storage).snd.fst;
  local type::Integer = head(storage).snd.snd;
  
  return
    if null(tail(storage))
    then 
      if type == storeDense
      then -- This is the last dimension, it is Dense
        ableC_Stmt {
          struct __tensor_tree* prev = tree->children;
          struct __tensor_tree* curr = prev->next;
          unsigned long i = 0;
          unsigned long cEnd = dims[$intLiteralExpr{dim}];
          while(curr) {
            if(curr->index != i) {
              struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));

              temp->isLeaf = 1;
              temp->index = i;
              temp->children = 0;
              temp->next = curr;
              prev->next = temp;
              curr = temp;
              (tree->numChildren)++;
            }
            i++;
            prev = curr;
            curr = curr->next;
          }
          while(i < cEnd) {
            struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));

            temp->isLeaf = 1;
            temp->index = i;
            temp->children = 0;
            temp->next = 0;
            prev->next = temp;
            (tree->numChildren)++;

            prev = temp;
            i++;
          }
        }
      else -- This is the last dimension, it is Sparse (nothing to do)
        nullStmt()
    else 
      if type == storeDense
      then -- This is not the last dimension, it is Dense
        ableC_Stmt {
          struct __tensor_tree* prev = tree->children;
          struct __tensor_tree* curr = prev->next;
          unsigned long i = 0;
          unsigned long cEnd = dims[$intLiteralExpr{dim}];
          while(curr) {
            if(curr->index != i) {
              struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));
              
              temp->isLeaf = 0;
              temp->index = i;
              temp->children = calloc(1, sizeof(struct __tensor_tree));
              temp->next = curr;
              prev->next = temp;
              curr = temp;
              (tree->numChildren)++;
            }
            struct __tensor_tree* tree = curr;
            {$Stmt{generatePackTreeBody(tail(storage))}} // Parentheses prevent name conflicts
            i++;
            prev = curr;
            curr = curr->next;
          }
          while(i < cEnd) {
            struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));

            temp->isLeaf = 0;
            temp->index = i;
            temp->children = calloc(1, sizeof(struct __tensor_tree));
            temp->next = 0;
            prev->next = temp;
            (tree->numChildren)++;

            struct __tensor_tree* tree = temp;
            {$Stmt{generatePackTreeBody(tail(storage))}}

            prev = temp;
            i++;
          }
        }
      else -- This is not the last dimension, it is Sparse
        ableC_Stmt {
          struct __tensor_tree* prev = tree->children;
          struct __tensor_tree* curr = prev->next;
          while(curr) {
            struct __tensor_tree* tree = curr;
            {$Stmt{generatePackTreeBody(tail(storage))}}
            prev = curr;
            curr = curr->next;
          }
        };
}

{- The pack function checks if anything is in the buffer. If there are things,
   it must acquire a write lock to be able to modify the tensor. After acquiring
   this lock, it checks that the buffer still is filled. If it has emptied while
   waiting, it releases the lock and exits. Otherwise, we add every element
   from the tensor's body into the buffer, and then pack the buffer. Finally,
   iterating through the buffer layer by layer, we build up the arrays to 
   keep track of elements.
-}
function declPack
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local order::Integer = fmt.dimensions;
  
  return 
    ableC_Decl {
      static void $name{s"tensor_pack_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* t) {
        if(t->bufferCnt == 0) return;

        pthread_rwlock_wrlock(&(t->lock));

        if(t->bufferCnt > 0) {
          unsigned long* dims = t->dims;
          unsigned long*** indices = t->indices;
          double* data = t->data;
          struct __tensor_tree* buffer = t->buffer;

          unsigned long pI0 = 0;
          unsigned long _index[$intLiteralExpr{fmt.dimensions}];
          $Stmt{generatePackBody_Tree(fmt.storage, fmtNm, fmt.dimensions, 1)}
          $name{s"tensor_packTree_${fmtNm}"}(buffer, dims);

          if(t->indices) { $Stmt{freeIndicesTPointer(fmt)} }
          t->indices = calloc($intLiteralExpr{order}, sizeof(unsigned long**));

          unsigned long numChildren = 1;
          struct __tensor_tree** trees = &(buffer);

          struct __tensor_tree** temp_tree;
          unsigned long total, dimSize, index, newChildren;

          $Stmt{generatePackBody_Assemble(fmt.storage)}

          if(t->data) free(t->data);
          t->data = calloc(numChildren, sizeof(double));

          for(unsigned long i = 0; i < numChildren; i++) {
            t->data[i] = trees[i]->val;
          }
          if(trees != &buffer) {
            free(trees);
          }

          __free_tensor_tree(t->buffer);
          t->dataLen = numChildren;
          t->bufferCnt = 0;
          t->buffer = calloc(1, sizeof(struct __tensor_tree));
          t->buffer->children = calloc(1, sizeof(struct __tensor_tree));
          t->form = "";
        }

        pthread_rwlock_unlock(&(t->lock));
      }
    };
}

{- Iterate over all elements in the tensor -}
function generatePackBody_Tree
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String order::Integer dim::Integer
{
  local dimen::Integer = head(storage).snd.fst;
  local type::Integer = head(storage).snd.snd;
  local str_dim::String = toString(dim);
  local dims::String = toString(dimen);
  
  return
    if null(storage)
    then 
      ableC_Stmt {
        double value = data[$name{s"pI${toString(dim-1)}"}];
        double* pnt = $name{s"tensor_insertZero_${fmtNm}"}(buffer, _index);
        if(*pnt == 0.0) *pnt = value;
      }
    else
      if type == storeDense
      then 
        ableC_Stmt {
          for(unsigned long $name{s"i${toString(dim)}"} = 0;
              $name{s"i${toString(dim)}"} < indices[$intLiteralExpr{dimen}][0][0];
              $name{s"i${toString(dim)}"}++) {
            unsigned long $name{s"pI${toString(dim)}"} = ($name{s"pI${toString(dim-1)}"} * indices[$intLiteralExpr{dimen}][0][0]) + $name{s"i${toString(dim)}"};
            _index[$intLiteralExpr{dimen}] = $name{s"i${toString(dim)}"};
            $Stmt{generatePackBody_Tree(tail(storage), fmtNm, order, dim+1)}
          }
        }
      else
        ableC_Stmt {
          for(unsigned long $name{s"pI${toString(dim)}"} = indices[$intLiteralExpr{dimen}][0][$name{s"pI${toString(dim-1)}"}];
              $name{s"pI${toString(dim)}"} < indices[$intLiteralExpr{dimen}][0][$name{s"pI${toString(dim-1)}"}+1];
              $name{s"pI${toString(dim)}"}++) {
            unsigned long $name{s"i${toString(dim)}"} = indices[$intLiteralExpr{dimen}][1][$name{s"pI${toString(dim)}"}];
            _index[$intLiteralExpr{dimen}] = $name{s"i${toString(dim)}"};
            $Stmt{generatePackBody_Tree(tail(storage), fmtNm, order, dim+1)}
          }
        };
}

{- Use the now packed buffer to fill in the indices array of the
   tensor. This also calculates the number of elements that 
   need to be allocated in the data array.
-}
function generatePackBody_Assemble
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dimInt::Integer = head(storage).snd.fst;
  local dim::String = toString(dimInt);
  local type::Integer = head(storage).snd.snd;
  
  return
    if null(storage)
    then nullStmt()
    else
      let inner :: Stmt =
        if type == storeDense
        then -- For dense, indices just stores the dimension
          ableC_Stmt {
            t->indices[$intLiteralExpr{dimInt}] = calloc(1, sizeof(unsigned long*));
            t->indices[$intLiteralExpr{dimInt}][0] = calloc(1, sizeof(unsigned long));
            t->indices[$intLiteralExpr{dimInt}][0][0] = dimSize;
          }
        else -- For sparse, indices store a description of what exists
          ableC_Stmt {
            t->indices[$intLiteralExpr{dimInt}] = calloc(2, sizeof(unsigned long*));
            t->indices[$intLiteralExpr{dimInt}][0] = calloc(numChildren + 1, sizeof(unsigned long));
            t->indices[$intLiteralExpr{dimInt}][1] = calloc(newChildren, sizeof(unsigned long));

            for(unsigned long k = 0; k < newChildren; k++) {
              t->indices[$intLiteralExpr{dimInt}][1][k] = temp_tree[k]->index;
            }
            total = 0;
            t->indices[$intLiteralExpr{dimInt}][0][0] = total;
            for(unsigned long k = 0; k < numChildren; k++) {
              total += trees[k]->numChildren;
              t->indices[$intLiteralExpr{dimInt}][0][k+1] = total;
            }
          }
      in
      ableC_Stmt {
        dimSize = dims[$intLiteralExpr{dimInt}];
        newChildren = 0;
        for(unsigned long j = 0; j < numChildren; j++) {
          struct __tensor_tree* prev = trees[j]->children;
          struct __tensor_tree* curr = prev->next;
          while(curr) {
            newChildren++;
            curr = curr->next;
          }
        }
        temp_tree = calloc(newChildren, sizeof(struct __tensor_tree*));
        index = 0;
        for(unsigned long j = 0; j < numChildren; j++) {
          struct __tensor_tree* prev = trees[j]->children;
          struct __tensor_tree* curr = prev->next;
          while(curr) {
            temp_tree[index] = curr;
            curr = curr->next;
            index++;
          }
        }

        $Stmt{inner} // What we do to build up the indices depends on the format

        if(trees != &buffer) {
          free(trees);
        }
        numChildren = newChildren;
        trees = temp_tree;
        $Stmt{generatePackBody_Assemble(tail(storage))}
      }
      end;
}
