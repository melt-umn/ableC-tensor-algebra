grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declMakeFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;

  return
    maybeValueDecl(
      s"tensor_make_${fmtNm}",
      declTensorMake(fmt)
    );
}

function declTensorMake
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    ableC_Decl {
      static void $name{s"tensor_make_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* t, unsigned long* dims) {
        t->dims = calloc($intLiteralExpr{dimens}, sizeof(unsigned long));
        memcpy(t->dims, dims, sizeof(unsigned long) * $intLiteralExpr{dimens});

        t->bufferCnt = 0;
        t->data = 0;
        t->indices = calloc($intLiteralExpr{dimens}, sizeof(unsigned long**));
        unsigned long count = 1;

        $Stmt{generateMakeBody(fmt.storage)}

        t->bufferCnt = 0;
        t->buffer = calloc(1, sizeof(struct __tensor_tree));
        t->buffer->children = calloc(1, sizeof(struct __tensor_tree));

        t->form = "";
        t->dataLen = count;
        pthread_rwlock_init(&(t->lock), 0);
      }
    };
}

function generateMakeBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>]
{
  return
    if null(storage)
    then 
      ableC_Stmt {
        t->data = calloc(count, sizeof(double));
      }
    else 
      let dim::Integer = head(storage).snd.fst in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then
        ableC_Stmt {
          t->indices[$intLiteralExpr{dim}] = calloc(1, sizeof(unsigned long*));
          t->indices[$intLiteralExpr{dim}][0] = calloc(1, sizeof(unsigned long));
          t->indices[$intLiteralExpr{dim}][0][0] = t->dims[$intLiteralExpr{dim}];
          count *= t->dims[$intLiteralExpr{dim}];
          $Stmt{generateMakeBody(tail(storage))}
        }
      else 
      ableC_Stmt {
        t->indices[$intLiteralExpr{dim}] = calloc(2, sizeof(unsigned long*));
        t->indices[$intLiteralExpr{dim}][0] = calloc(count + 1, sizeof(unsigned long));
        count = 1;
        $Stmt{generateMakeBody(tail(storage))}
      }
      end
      end;
}
