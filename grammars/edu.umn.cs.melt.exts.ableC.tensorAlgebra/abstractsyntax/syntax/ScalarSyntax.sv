grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:syntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production format_scalar
top::Decl ::= nm::Name
{
  local errors::[Message] =
    nm.tensorFormatRedeclarationCheck;
  
  propagate substituted;
  top.pp = ppConcat([
             text("tensor format "),
             text(nm.name),
             text(" = ({});")
           ]);
  
  local fmt::TensorFormatItem = tensorScalar(nm.location);
  
  local fwrd::Decl =
    decls(consDecl(
      declAllScalar(),
      consDecl(
        defsDecl([tensorFormatDef(nm.name, fmt)]),
        nilDecl()
      )
    ));
  
  forwards to
  if !null(errors)
  then warnDecl(errors)
  else fwrd;
}

abstract production scalar_new
top::Expr ::= type::TypeName
{
  local formatType::Type = type.typerep;
  
  local format::Name =
    case formatType of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  top.pp = text("build (tensor<${format.name}>) ()");
  
  propagate substituted;
  
  local fmt::Decorated TensorFormatItem = format.tensorFormatItem;

  local errors::[Message] =
    case formatType of
    | pointerType(_,
        tensorType(_, _, _)) ->
          format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type.")]
    end;
  
  local fwrd::Expr =
    parseExpr(s"""
    ({
      struct tensor_scalar* _tensor = GC_malloc(sizeof(struct tensor_scalar));
      _tensor->data = GC_malloc(sizeof(double));
      _tensor;
    })
    """);
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production scalar_get
top::Expr ::= tensor::Expr
{
  top.pp = ppConcat([
             text("value ("),
             tensor.pp,
             text(")()")
           ]);

  local errors::[Message] =
    case tensor.typerep of
    | pointerType(_,
        tensorType(_, _, _))
      -> []
    | _ -> [err(tensor.location, s"Tensor value expected a tensor (got ${showType(tensor.typerep)})")]
    end
    ++
    case tensor of
    | errorExpr(errs) -> errs
    | _ -> []
    end;
  
  propagate substituted;
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__tensor", tensor) :: [],
      parseExpr(s"""
      ({
        __tensor->data[0];
      })
      """));
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production scalar_assign
top::Stmt ::= tensor::Expr op::AssignmentOp val::Expr
{
  top.pp = ppConcat([
             text("value ("),
             tensor.pp,
             text(")()"),
             text(op.rep),
             val.pp,
             text(";")
           ]);

  local errors::[Message] =
    case tensor of
    | errorExpr(errs) -> errs
    | _ ->
      case tensor.typerep of
      | pointerType(_,
          tensorType(_, _, _))
          -> []
      | _ -> [err(tensor.location, s"Tensor value expected a tensor (got ${showType(tensor.typerep)})")]
      end
    end;
  
  propagate substituted;
  top.functionDefs := [];
  
  local fwrd::Stmt =
    substStmt(
      declRefSubstitution(s"__tensor", tensor) ::
      declRefSubstitution(s"__value", val) :: []
      ,
      scalar_assignment(op)
    );
  
  forwards to
    if null(errors)
    then fwrd
    else warnStmt(errors);
}

function declAllScalar
Decl ::= 
{
  return
    decls(
      consDecl(
        declScalarStruct(),
        consDecl(
          declScalarPack(),
          nilDecl()
        )
      )
    );
}

function declScalarStruct
Decl ::=
{
  return maybeTagDecl(
         s"tensor_scalar",
         parseDecl(s"""
           struct __attribute__((refId("edu:umn:cs:melt:exts:ableC:tensorAlgebra:tensor_scalar"))) tensor_scalar {
             double* data;
             char* form;
           };
         """)
         );
}

function declScalarPack
Decl ::=
{
  return maybeValueDecl(
           s"tensor_pack_scalar",
           parseDecl(s"""
             static void tensor_pack_scalar(struct tensor_scalar* t) {
               
             }
           """)
         );
}

function scalar_assignment
Stmt ::= op::AssignmentOp
{
  return
    parseStmt(s"""
    {
      struct tensor_scalar* _tensor = __tensor;
      double _value = __value;
      _tensor->data[0] ${op.rep} __value;
    }
    """);
}
