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
    parseDecl(s"""
      static double tensor_get_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index) 
      {
        pthread_rwlock_rdlock(&(t->lock));

        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
        
        unsigned long start, end;
        
        ${generateIndexCheck(dimens)}
        
        ${generateGetBody(fmt.storage, fmtNm)}
      }
    """);
}

function generateGetBody
String ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then s"""
      double res = data[pTI];
      pthread_rwlock_unlock(&(t->lock));
      return res;
    """
    else 
      let dim::Integer = head(storage).snd.fst in
      let dimen::String = toString(dim) in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then s"""
        pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
        ${generateGetBody(tail(storage), fmtNm)}
      """
      else s"""
        found = 0;
        start = indices[${dimen}][0][pTI];
        end = indices[${dimen}][0][pTI + 1];
        for(unsigned long j = start; j < end; j++) {
          if(indices[${dimen}][1][j] == index[${dimen}]) {
            pTI = j;
            found = 1;
            break;
          }
        }
        if(!found) {
          pthread_rwlock_unlock(&(t->lock));
          return 0.0;
        }
        ${generateGetBody(tail(storage), fmtNm)}
      """
      end
      end
      end;
}


function declGetPointerFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    parseDecl(s"""
      static double* tensor_getPointer_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index) 
      {
        pthread_rwlock_wrlock(&(t->lock));

        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
       
        unsigned long start, end;
        
        ${generateIndexCheck(dimens)}
        
        ${generateGetPointerBody(fmt.storage, fmtNm)}
      }
    """);
}

function generateGetPointerBody
String ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then s"""
      double* res = data + pTI;
      pthread_rwlock_unlock(&(t->lock));
      return res;
    """
    else 
      let dim::Integer = head(storage).snd.fst in
      let dimen::String = toString(dim) in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then s"""
        pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
        ${generateGetPointerBody(tail(storage), fmtNm)}
      """
      else s"""
        found = 0;
        start = indices[${dimen}][0][pTI];
        end = indices[${dimen}][0][pTI + 1];
        for(unsigned long j = start; j < end; j++) {
          if(indices[${dimen}][1][j] == index[${dimen}]) {
            pTI = j;
            found = 1;
            break;
          }
        }
        if(!found) {
          double* res = tensor_insertZero_${fmtNm}(&(t->buffer), index);
          t->bufferCnt++;
          pthread_rwlock_unlock(&(t->lock));
          return res;
        }
        ${generateGetPointerBody(tail(storage), fmtNm)}
      """
      end
      end
      end;
}

function declGetPointerLockedFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  local dimens::Integer = fmt.dimensions;
  
  return
    parseDecl(s"""
      static double* tensor_getPointer_locked_${fmtNm}(struct tensor_${fmtNm}* t, unsigned long* index) 
      {
        unsigned long* dims = t->dims;
        unsigned long*** indices = t->indices;
        double* data = t->data;
        unsigned long pTI = 0;
        char found = 0;
       
        unsigned long start, end;
        
        ${generateIndexCheck(dimens)}
        
        ${generateGetPointerLockedBody(fmt.storage, fmtNm)}
      }
    """);
}

function generateGetPointerLockedBody
String ::= storage::[Pair<Integer Pair<Integer Integer>>] fmtNm::String
{
  return
    if null(storage)
    then s"""
      double* res = data + pTI;
      return res;
    """
    else 
      let dim::Integer = head(storage).snd.fst in
      let dimen::String = toString(dim) in
      let spec::Integer = head(storage).snd.snd in
      if spec == storeDense
      then s"""
        pTI = (pTI * indices[${dimen}][0][0]) + index[${dimen}];
        ${generateGetPointerLockedBody(tail(storage), fmtNm)}
      """
      else s"""
        found = 0;
        start = indices[${dimen}][0][pTI];
        end = indices[${dimen}][0][pTI + 1];
        for(unsigned long j = start; j < end; j++) {
          if(indices[${dimen}][1][j] == index[${dimen}]) {
            pTI = j;
            found = 1;
            break;
          }
        }
        if(!found) {
          double* res = tensor_insertZero_${fmtNm}(&(t->buffer), index);
          t->bufferCnt++;
          return res;
        }
        ${generateGetPointerLockedBody(tail(storage), fmtNm)}
      """
      end
      end
      end;
}
