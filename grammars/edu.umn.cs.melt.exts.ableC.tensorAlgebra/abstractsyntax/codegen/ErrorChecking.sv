grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen;

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
