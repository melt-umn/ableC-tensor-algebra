grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

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

function generateIndexInitializer
Initializer ::= size::Integer
{
  return
    objectInitializer(generateIndexInitList(1, size));
}

function generateIndexInitList
InitList ::= idx::Integer size::Integer
{
  return
    if idx == size
    then
      consInit(
        positionalInit(
          exprInitializer(
            ableC_Expr {
              $name{s"i${toString(idx)}"}
            }
          )
        ),
        nilInit()
      )
    else
      consInit(
        positionalInit(
          exprInitializer(
            ableC_Expr {
              $name{s"i${toString(idx)}"}
            }
          )
        ),
        generateIndexInitList(idx+1, size)
      );
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
Expr ::= dims::Integer idx::Integer
{
  local index::String = toString(idx);
  
  return
    if dims == idx
    then 
      ableC_Expr {
        1
      }
    else 
      ableC_Expr {
        dims[$intLiteralExpr{idx}] * $Expr{generateProductDims(dims, idx+1)}
      };
}

function freeIndicesTPointer
Stmt ::= fmt::TensorFormat
{
  return freeIndicesTPointer_helper(fmt.storage);
}

function freeIndicesTPointer_helper
Stmt ::= strg::[Pair<Integer Pair<Integer Integer>>]
{
  local p :: Pair<Integer Pair<Integer Integer>> =
    head(strg);

  return
    if null(strg)
    then
      ableC_Stmt {
        free(t->indices);
      }
    else if p.snd.snd == storeDense
    then
      ableC_Stmt {
        free(t->indices[$intLiteralExpr{p.snd.fst}][0]);
        free(t->indices[$intLiteralExpr{p.snd.fst}]);
        $Stmt{freeIndicesTPointer_helper(tail(strg))}
      }
    else
      ableC_Stmt {
        free(t->indices[$intLiteralExpr{p.snd.fst}][0]);
        free(t->indices[$intLiteralExpr{p.snd.fst}][1]);
        free(t->indices[$intLiteralExpr{p.snd.fst}]);
        $Stmt{freeIndicesTPointer_helper(tail(strg))}
      };
}
