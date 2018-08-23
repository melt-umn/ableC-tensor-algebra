grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

-- Check that a requested index (in the variable index) is valid in the
-- tensor that has it's dimensions in the array dims.
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
          fprintf(stderr, $stringLiteralExpr{s"Invalid index on dimension ${toString(idx)} of tensor (index %lu requested, size %lu). (%s)\n"}, index[$intLiteralExpr{idx}], dims[$intLiteralExpr{idx}], __tensor_location);
          err = 1;
        }
        $Stmt{indexCheckStmtBody(dims, idx + 1)}
      };
}

-- Generate an array initializer for the values
-- i1, i2, ...
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

{- Generates code to free the indices of a tensor stored in the
   specified format. This function assumes that the tensor is
   pointed to by 't'
-}
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
