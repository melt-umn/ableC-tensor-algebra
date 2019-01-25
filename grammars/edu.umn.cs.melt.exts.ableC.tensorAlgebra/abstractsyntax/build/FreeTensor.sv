grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

{- Production for properly free'ing a tensor. Because this depends on the
   storage format it is done at compile time. Technically it probably
   can be done at run-time, but this would require malloc'ing more memory
   than necessary in a few places.
-}
abstract production freeTensor
top::Expr ::= tensor::Expr
{
  propagate substituted;
  top.pp = 
    ppConcat([
      text("freeTensor("),
      tensor.pp,
      text(")")
    ]);

  local format::Name =
    case tensor.typerep of
    | extType(_, tensorType(fmt)) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;

  local lErrors :: [Message] =
    case tensor.typerep of
    | extType(_, tensorType(_)) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"freeTensor expected a tensor type (got ${showType(tensor.typerep)})")]
    end;

  local fmt::TensorFormat =
    new(format.tensorFormat);

  local fmtNm::String =
    fmt.proceduralName;

  forwards to
    mkErrorCheck(lErrors,
      ableC_Expr {
      ({
        struct $name{s"tensor_${fmtNm}"}* _tensor = (struct $name{s"tensor_${fmtNm}"}*) &$Expr{tensor};
        free(_tensor->data);
        free(_tensor->dims);
        $Stmt{freeIndices(ableC_Expr{*_tensor}, fmt)}
        free(_tensor->indices);
        if(_tensor->buffer) __free_tensor_tree(_tensor->buffer);
        pthread_rwlock_destroy(&(_tensor->lock));
        1;
      })
      });
}
