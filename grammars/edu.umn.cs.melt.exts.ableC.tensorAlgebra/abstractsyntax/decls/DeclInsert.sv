grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Here we declare two functions for working with a tensor's buffer.
   tensor_insertBuff, inserts a specified value into the buffer at the 
   specified location. tensor_insertZero, inserts a zero into the buffer
   at the specified location and returns a pointer to the value. This
   is used in tensor_getPointer when an element is not found.
-}
function declInsertFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;

  return
    decls(consDecl(
      maybeValueDecl(
        s"tensor_insertBuff_${fmtNm}",
        declInsertFunc(fmt)
      ),
      consDecl(
        maybeValueDecl(
          s"tensor_insertZero_${fmtNm}",
          declInsertZeroFunction(fmt)
        ),
        nilDecl()
      )
    ));
}

function declInsertFunc
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    ableC_Decl {
      static void $name{s"tensor_insertBuff_${fmtNm}"}(struct __tensor_tree* tree, unsigned long* index, double val) {
        unsigned long idx, currIdx;
        char found = 0;
        $Stmt{generateInsertBody(fmt.storage)}
      }
    };
}

function generateInsertBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dim::Integer = head(storage).snd.fst;
  
  return
    if null(storage)
    then 
      ableC_Stmt {
        tree->isLeaf = 1;
        tree->val = val;
        tree->index = idx;
      }
    else 
      ableC_Stmt {
        idx = index[$intLiteralExpr{dim}];
        struct __tensor_tree* prev = tree->children;
        struct __tensor_tree* curr = prev->next;
        found = 0;
        currIdx = -1;
        while(!found &&  currIdx < idx && curr) {
          currIdx = curr->index;
          if(currIdx == idx) {
            tree = curr;
            found = 1;
          } else if(currIdx < idx) {
            prev = curr;
            curr = curr->next;
          }
        }
        if(!found) {
          struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));
          
          temp->isLeaf = 0;
          temp->index = idx;
          temp->children = calloc(1, sizeof(struct __tensor_tree));
          temp->next = curr;
          prev->next = temp;

          (tree->numChildren)++;
          tree = temp;
        }
        {$Stmt{generateInsertBody(tail(storage))}}
      };
}

function declInsertZeroFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return 
    ableC_Decl {
      static double* $name{s"tensor_insertZero_${fmtNm}"}(struct __tensor_tree* tree, unsigned long* index) {
        unsigned long idx, currIdx;
        char found = 0;
        $Stmt{generateInsertZeroBody(fmt.storage)}
      }
    };
}

function generateInsertZeroBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dim::Integer = head(storage).snd.fst;
  
  return
    if null(storage)
    then 
      ableC_Stmt {
        tree->isLeaf = 1;
        tree->index = idx;
        return &(tree->val);
      }
    else 
      ableC_Stmt {
        idx = index[$intLiteralExpr{dim}];
        struct __tensor_tree* prev = tree->children;
        struct __tensor_tree* curr = prev->next;
        found = 0;
        currIdx = 0;
        while(!found && curr && currIdx <= idx) {
          currIdx = curr->index;
          if(currIdx == idx) {
            tree = curr;
            found = 1;
          } else if(currIdx < idx) {
            prev = curr;
            curr = curr->next;
          }
        }
        if(!found) {
          struct __tensor_tree* temp = calloc(1, sizeof(struct __tensor_tree));
          
          temp->isLeaf = 0;
          temp->index = idx;
          temp->children = calloc(1, sizeof(struct __tensor_tree));
          temp->next = curr;
          prev->next = temp;

          (tree->numChildren)++;
          tree = temp;
        }
        {$Stmt{generateInsertZeroBody(tail(storage))}}
      };
}
