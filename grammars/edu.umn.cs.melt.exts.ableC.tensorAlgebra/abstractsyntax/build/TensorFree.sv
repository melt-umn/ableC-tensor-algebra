grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

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
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;

  local lErrors :: [Message] =
    case tensor.typerep of
    | tensorType(_, _, _) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"freeTensor expected a tensor type (got ${showType(tensor.typerep)})")]
    end;

  local fmt::TensorFormat =
    new(format.tensorFormat);

  forwards to
    ableC_Expr {
    ({
      free($Expr{tensor}.data);
      free($Expr{tensor}.dims);
      $Stmt{freeIndices(tensor, fmt)}
      free($Expr{tensor}.indices);
      if($Expr{tensor}.buffer) __free_tensor_tree($Expr{tensor}.buffer);
      pthread_rwlock_destroy(&($Expr{tensor}.lock));
      1;
    })
    };
}
