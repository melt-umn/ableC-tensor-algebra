grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declTensorGet
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    decls(consDecl(
      maybeValueDecl(
        s"tensor_get_${fmtNm}",
        declGetFunction(fmt)
      ),
      consDecl(
        maybeValueDecl(
          s"tensor_getPointer_${fmtNm}",
          declGetPointerFunction(fmt)
        ),
        consDecl(
          maybeValueDecl(
            s"tensor_getPointer_locked_${fmtNm}",
            declGetPointerLockedFunction(fmt)
          ),
          nilDecl()
        )
      )
    ));
}

function declGetFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    ableC_Decl {
      static double $name{s"tensor_get_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* t, unsigned long* index) 
      {
        pthread_rwlock_rdlock(&(t->lock));

        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
        
        unsigned long start, end;
        
        $Stmt{indexCheckStmt(dimens)}
        
        $Stmt{generateGetBody(fmt.storage, fmtNm)}
      }
    };
}

function generateGetBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then
      ableC_Stmt {
        double res = data[pTI];
        pthread_rwlock_unlock(&(t->lock));
        return res;
      }
    else 
      let dim::Integer = head(storage).snd.fst in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then
        ableC_Stmt {
          pTI = (pTI * indices[$intLiteralExpr{dim}][0][0]) + index[$intLiteralExpr{dim}];
          $Stmt{generateGetBody(tail(storage), fmtNm)}
        }
      else 
        ableC_Stmt {
          found = 0;
          start = indices[$intLiteralExpr{dim}][0][pTI];
          end = indices[$intLiteralExpr{dim}][0][pTI + 1];
          for(unsigned long j = start; j < end; j++) {
            if(indices[$intLiteralExpr{dim}][1][j] == index[$intLiteralExpr{dim}]) {
              pTI = j;
              found = 1;
              break;
            }
          }
          if(!found) {
            pthread_rwlock_unlock(&(t->lock));
            return 0.0;
          }
          $Stmt{generateGetBody(tail(storage), fmtNm)}
        }
      end
      end;
}


function declGetPointerFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    ableC_Decl {
      static double* $name{s"tensor_getPointer_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* t, unsigned long* index) 
      {
        pthread_rwlock_wrlock(&(t->lock));

        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
       
        unsigned long start, end;
        
        $Stmt{indexCheckStmt(dimens)}
        
        $Stmt{generateGetPointerBody(fmt.storage, fmtNm)}
      }
    };
}

function generateGetPointerBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then
      ableC_Stmt {
        double* res = data + pTI;
        pthread_rwlock_unlock(&(t->lock));
        return res;
      }
    else 
      let dim::Integer = head(storage).snd.fst in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then 
        ableC_Stmt {
          pTI = (pTI * indices[$intLiteralExpr{dim}][0][0]) + index[$intLiteralExpr{dim}];
          $Stmt{generateGetPointerBody(tail(storage), fmtNm)}
        }
      else
        ableC_Stmt {
          found = 0;
          start = indices[$intLiteralExpr{dim}][0][pTI];
          end = indices[$intLiteralExpr{dim}][0][pTI + 1];
          for(unsigned long j = start; j < end; j++) {
            if(indices[$intLiteralExpr{dim}][1][j] == index[$intLiteralExpr{dim}]) {
              pTI = j;
              found = 1;
              break;
            }
          }
          if(!found) {
            double* res = $name{s"tensor_insertZero_${fmtNm}"}(&(t->buffer), index);
            t->bufferCnt++;
            pthread_rwlock_unlock(&(t->lock));
            return res;
          }
          $Stmt{generateGetPointerBody(tail(storage), fmtNm)}
        }
      end
      end;
}

function declGetPointerLockedFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    ableC_Decl {
      static double* $name{s"tensor_getPointer_locked_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* t, unsigned long* index) 
      {
        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
       
        unsigned long start, end;
        
        $Stmt{indexCheckStmt(dimens)}
        
        $Stmt{generateGetPointerLockedBody(fmt.storage, fmtNm)}
      }
    };
}

function generateGetPointerLockedBody
Stmt ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then
      ableC_Stmt {
        double* res = data + pTI;
        return res;
      }
    else 
      let dim::Integer = head(storage).snd.fst in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then
        ableC_Stmt {
          pTI = (pTI * indices[$intLiteralExpr{dim}][0][0]) + index[$intLiteralExpr{dim}];
          $Stmt{generateGetPointerLockedBody(tail(storage), fmtNm)}
        }
      else
        ableC_Stmt {
          found = 0;
          start = indices[$intLiteralExpr{dim}][0][pTI];
          end = indices[$intLiteralExpr{dim}][0][pTI + 1];
          for(unsigned long j = start; j < end; j++) {
            if(indices[$intLiteralExpr{dim}][1][j] == index[$intLiteralExpr{dim}]) {
              pTI = j;
              found = 1;
              break;
            }
          }
          if(!found) {
            double* res = $name{s"tensor_insertZero_${fmtNm}"}(&(t->buffer), index);
            t->bufferCnt++;
            return res;
          }
          $Stmt{generateGetPointerLockedBody(tail(storage), fmtNm)}
        }
      end
      end;
}
