grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

function generateIndexCheck
String ::= dims::Integer
{
  return s"""
    char err = 0;
    ${generateIndexCheckBody(dims, 0)}
  """;
}

function generateIndexCheckBody
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if dims == idx
    then s"""
      if(err) {
        exit(1);
      }
    """
    else s"""
      if(index[${index}] >= dims[${index}]) {
        fprintf(stderr, "Invalid index on dimension ${index} of tensor, (index %lu, size %lu)\n", index[${index}], dims[${index}]);
        err = 1;
      }
      ${generateIndexCheckBody(dims, idx + 1)}
    """;
}

function indexCheckStmt
Stmt ::= dims::Integer
{
  return
    ableC_Stmt {
      char err = 0;
      $Stmt{indexCheckStmtBody(dims, 0)}
    };
}

function indexCheckStmtBody
Stmt ::= dims::Integer idx::Integer
{
  return
    if dims == idx
    then 
      ableC_Stmt {
        if(err) {
          exit(1);
        }
      }
    else
      ableC_Stmt {
        if(index[$intLiteralExpr{idx}] >= dims[$intLiteralExpr{idx}]) {
          fprintf(stderr, $stringLiteralExpr{s"Invalid index on dimension ${toString(idx)} of tensor (index %lu requested, size %lu)\n"}, index[$intLiteralExpr{idx}], dims[$intLiteralExpr{idx}]);
          err = 1;
        }
        $Stmt{indexCheckStmtBody(dims, idx + 1)}
      };
}

function generateIndexArray
String ::= size::Integer
{
  return
    if size == 1
    then "i1"
    else s"${generateIndexArray(size-1)}, i${toString(size)}";
}

function generateProductDims
String ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if dims == idx
    then "1"
    else s"dims[${index}] * ${generateProductDims(dims, idx + 1)}";
}
