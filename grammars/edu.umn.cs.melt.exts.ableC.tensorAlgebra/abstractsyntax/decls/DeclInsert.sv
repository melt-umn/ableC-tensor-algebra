grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

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
          s"tensor_insertBuff_mid_${fmtNm}",
          declInsertMidFunction(fmt)
        ),
        consDecl(
          maybeValueDecl(
            s"tensor_insertZero_${fmtNm}",
            declInsertZeroFunction(fmt)
          ),
          nilDecl()
        )
      )
    ));
}

function declInsertFunc
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    ableC_Decl {
      static void $name{s"tensor_insertBuff_${fmtNm}"}(struct tensor_tree_s* tree, unsigned long* index, double val) {
        unsigned long idx, end, i, currIdx;
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
        end = tree->numChildren;
        found = 0;
        i = 0;
        currIdx = 0;
        while(!found && i < end && currIdx <= idx) {
          currIdx = tree->children[i].index;
          if(currIdx == idx) {
            tree = &(tree->children[i]);
            found = 1;
          } else if(currIdx < idx) {
            i++;
          }
        }
        if(!found) {
          struct tensor_tree_s* temp = calloc(end + 1, sizeof(struct tensor_tree_s));
          
          memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
          
          temp[i].isLeaf = 0;
          temp[i].index = idx;
          temp[i].numChildren = 0;
          
          memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
          
          tree->numChildren += 1;
          if(tree->children) free(tree->children);
          tree->children = temp;
          tree = temp + i;
        }
        $Stmt{generateInsertBody(tail(storage))}
      };
}


function declInsertMidFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return 
    ableC_Decl {
      static void $name{s"tensor_insertBuff_mid_${fmtNm}"}(struct tensor_tree_s* tree, unsigned long* index, unsigned long level) {
        unsigned long idx, end, i, currIdx, currLevel = 0;
        char found = 0;
        $Stmt{generateInsertMidBody(fmt.storage)}
      }
    };
}

function generateInsertMidBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  local dim::Integer = head(storage).snd.fst;
  
  return
    if null(storage)
    then
      ableC_Stmt {
        tree->isLeaf = 1;
        tree->index = idx;
        tree->val = 0.0;
      }
    else
      ableC_Stmt {
        if(currLevel <= level) {
          idx = index[$intLiteralExpr{dim}];
          end = tree->numChildren;
          found = 0;
          i = 0;
          currIdx = 0;
          while(!found && i < end && currIdx <= idx) {
            currIdx = tree->children[i].index;
            if(currIdx == idx) {
              tree = &(tree->children[i]);
              found = 1;
            } else if(currIdx < idx) {
              i++;
            }
          }
          if(!found) {
            struct tensor_tree_s* temp = calloc(end + 1, sizeof(struct tensor_tree_s));
            
            memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
            
            temp[i].isLeaf = 0;
            temp[i].index = idx;
            temp[i].numChildren = 0;
            
            memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
            
            tree->numChildren += 1;
            if(tree->children) free(tree->children);
            tree->children = temp;
            tree = temp + i;
          }
          
          currLevel++;
          $Stmt{generateInsertMidBody(tail(storage))}
        }
      };
}

function declInsertZeroFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return 
    ableC_Decl {
      static double* $name{s"tensor_insertZero_${fmtNm}"}(struct tensor_tree_s* tree, unsigned long* index) {
        unsigned long idx, end, i, currIdx;
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
        end = tree->numChildren;
        found = 0;
        i = 0;
        currIdx = 0;
        while(!found && i < end && currIdx <= idx) {
          currIdx = tree->children[i].index;
          if(currIdx == idx) {
            tree = tree->children + i;
            found = 1;
          } else if(currIdx < idx) {
            i++;
          }
        }
        if(!found) {
          struct tensor_tree_s* temp = calloc(end + 1, sizeof(struct tensor_tree_s));
          
          memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
          
          temp[i].isLeaf = 0;
          temp[i].index = idx;
          temp[i].numChildren = 0;
          
          memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
          
          tree->numChildren += 1;
          if(tree->children) free(tree->children);
          tree->children = temp;
          tree = temp + i;
        }
        $Stmt{generateInsertZeroBody(tail(storage))}
      };
}
