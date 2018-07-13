grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declInsertFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return 
    decls(consDecl(
      maybeValueDecl(
        s"tensor_insertBuff_${fmtNm}",
        parseDecl(generateInsertFunction(fmt))
      ),
      consDecl(
        maybeValueDecl(
          s"tensor_insertBuff_mid_${fmtNm}",
          parseDecl(generateInsertFunctionMid(fmt))
        ),
        nilDecl()
      )
    ));
}

function generateInsertFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static void tensor_insertBuff_${fmtNm}(struct tensor_tree_s* tree, unsigned long* index, double val) {
      unsigned long idx, end, i, currIdx;
      char found = 0;
      ${generateInsertBody(fmt.dimenOrder, fmt.dimens)}
    }
  """;
}

function generateInsertBody
String ::= order::[Integer] dimensions::Integer
{
  local dim::Integer = head(order);
  
  return 
    if !null(order)
    then s"""
      idx = index[${toString(dim)}];
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
        struct tensor_tree_s* temp = GC_malloc(sizeof(struct tensor_tree_s) * (end + 1));

        memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
        
        temp[i].isLeaf = 0;
        temp[i].index = idx;
        temp[i].numChildren = 0;
        
        memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
        
        tree->numChildren += 1;
        tree->children = temp;
        tree = &(temp[i]);
      }
      ${generateInsertBody(tail(order), dimensions)}
    """
    else s"""
      tree->isLeaf = 1;
      tree->val = val;
      tree->index = idx;
    """;
}

function generateInsertFunctionMid
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static void tensor_insertBuff_mid_${fmtNm}(struct tensor_tree_s* tree, unsigned long* index, unsigned long level) {
      unsigned long idx, end, i, currIdx, currLevel = 0;
      char found = 0;
      ${generateInsertMidBody(fmt.dimenOrder, fmt.dimens)}
    }
  """;
}

function generateInsertMidBody
String ::= order::[Integer] dimensions::Integer
{
  local dim::Integer = head(order);
  
  return
    if !null(order)
    then s"""
      if(currLevel <= level) {
        idx = index[${toString(dim)}];
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
          struct tensor_tree_s* temp = GC_malloc(sizeof(struct tensor_tree_s) * (end + 1));
          
          memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
          
          temp[i].isLeaf = 0;
          temp[i].index = idx;
          temp[i].numChildren = 0;
          
          memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
          
          tree->numChildren += 1;
          tree->children = temp;
          tree = &(temp[i]);
        }
        
        currLevel++;
        ${generateInsertMidBody(tail(order), dimensions)}
      }
    """
    else s"""
      tree->isLeaf = 1;
      tree->index = idx;
      tree->val = 0.0;
    """;
}

function generateInsertZeroFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return s"""
    static void tensor_insertBuff_zero_${fmtNm}(struct tensor_tree_s*, unsigned long* index) {
      unsigned long idx, end, i, currIdx;
      char found = 0;
      ${generateInsertZeroBody(fmt.dimenOrder, fmt.dimens)}
    }
  """;
}

function generateInsertZeroBody
String ::= order::[Integer] dimensions::Integer
{
  local dim::Integer = head(order);
  
  return
    if !null(order)
    then s"""
      idx = index[${toString(dim)}];
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
        struct tensor_tree_s* temp = GC_malloc(sizeof(struct tensor_tree_s) * (end + 1));
        
        memcpy(temp, tree->children, sizeof(struct tensor_tree_s) * i);
        
        temp[i].isLeaf = 0;
        temp[i].index = idx;
        temp[i].numChildren = 0;
        
        memcpy(temp + i + 1, tree->children + i, sizeof(struct tensor_tree_s) * (end - i));
        
        tree->numChidren += 1;
        tree->children = temp;
        tree = temp + i;
      }
      ${generateInsertZeroBody(tail(order), dimensions)}
    """
    else s"""
      tree->isLeaf = 1;
      tree->val = 0.0;
      tree->index = idx;
      return &(tree->val);
    """;
}
