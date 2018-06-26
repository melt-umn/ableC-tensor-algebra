grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declInsertFunction
Decl ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;

  return maybeValueDecl(
    s"tensor_insertBuff_${fmtNm}",
    parseDecl(generateInsertFunction(fmt))
  );
}

function generateInsertFunction
String ::= fmt::TensorFormatItem
{
  local fmtNm::String = fmt.proceduralName;
  
  return s"""
    static void tensor_insertBuff_${fmtNm}(struct tensor_tree_s* tree, unsigned long* index, double val) {
      unsigned long idx, end, found, i, currIdx;
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
        }
        i++;
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
