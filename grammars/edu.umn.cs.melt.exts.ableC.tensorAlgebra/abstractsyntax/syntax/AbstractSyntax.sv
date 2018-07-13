grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:syntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production format_without
top::Decl ::= nm::Name types::[Integer]
{
  local errors::[Message] =
    case types of
    | [] -> [err(nm.location, s"Cannot have zero dimensional tensor. Not supported")]
    | _ -> []
    end ++ nm.tensorFormatRedeclarationCheck;
  
  propagate substituted;
  top.pp = ppConcat([
             text("tensor format "),
             text(nm.name),
             text(" = ({"),
             ppImplode(text(", "), map(\a::Integer -> text(if a == storeDense
                                                           then "dense"
                                                           else "sparse"), types)),
             text("});")
           ]);

  local order::[Integer] = defaultOrder(listLength(types));
  local fmt::TensorFormatItem = tensorFormatItem(types, order, nm.location);
  
  local fwrd::Decl =
    decls(consDecl(
      declAll(fmt),
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

abstract production format_with
top::Decl ::= nm::Name types::[Integer] order::[Integer]
{
  local dimens::Integer = listLength(order);

  local errors::[Message] =
    case types of
    | [] -> [err(nm.location, s"Cannot have zero dimensional tensor. Not supported")]
    | _ -> []
    end ++
    if listLength(types) != dimens
    then [err(nm.location, s"Tensor format must have same number of specified dimensions in type and order.")]
    else []
    ++
    if !checkOrder(order)
    then [err(nm.location, s"Tensor format's order must contain each dimension once, represented as a number 0, 1, ..., d-1.")]
    else []
    ++
    nm.tensorFormatRedeclarationCheck;
  
  propagate substituted;
  top.pp = ppConcat([
             text("tensor format "),
             text(nm.name),
             text(" = ({"),
             ppImplode(text(", "), map(\a::Integer -> text(if a == storeDense
                                                           then "dense"
                                                           else "sparse"), types)),
             text("}, {"),
             ppImplode(text(", "), map(\a::Integer -> text(toString(a)), types)),
             text("});")
          ]);
  
  local fmt::TensorFormatItem = tensorFormatItem(types, order, nm.location);
  
  local fwrd::Decl =
    decls(consDecl(
      declAll(fmt),
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

abstract production tensor_empty
top::Expr ::= type::TypeName dims::[Expr]
{
  local formatType::Type = type.typerep;

  local format::Name =
    case formatType of
    | pointerType(_, 
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  top.pp = ppConcat([
             text("build (tensor<${format.name}>) ({"),
             ppImplode(text(", "), map((.pp), dims)),
             text("})")
           ]);
  
  propagate substituted;
  
  local fmt::Decorated TensorFormatItem = format.tensorFormatItem;
  local dimens::Integer = fmt.dimens;
  
  local errors::[Message] = 
    case formatType of
    | pointerType(_,
        tensorType(_, _, _)) -> 
      format.tensorFormatLookupCheck
      ++
      if dimens > 0 && dimens != listLength(dims)
      then [err(top.location, s"Number of dimensions specified does not match format.")]
      else []
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type.")]
    end
    ++
    errorCheckTypes(head(dims), tail(dims), top.env);
    
  local fmtNm::String = fmt.proceduralName;
    
  local fwrd::Expr =
    substExpr(
      generateSubstitutions(dims, 0),
      parseExpr(s"""
      ({
        unsigned long __tensor_arr[] = {${generateArray(dims, 0)}};
        struct tensor_${fmtNm}* _tensor = tensor_make_${fmtNm}(__tensor_arr);
        _tensor;
      })
      """)
    );

  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production tensor_filled
top::Expr ::= type::TypeName data::Tensor
{
  local formatType::Type = type.typerep;
  
  local format::Name =
    case formatType of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  top.pp = ppConcat([
             text("build (tensor<${format.name}>) ("),
             data.pp,
             text(")")
           ]);

  propagate substituted;
  
  local fmt::Decorated TensorFormatItem = format.tensorFormatItem;
  local dimens::Integer = fmt.dimens;
  
  data.pos = "";
  data.env = top.env;
  
  local errors::[Message] =
    case formatType of
    | pointerType(_,
        tensorType(_, _, _)) ->
      format.tensorFormatLookupCheck
      ++
      if dimens > 0 && dimens != data.dimensions
      then [err(top.location, s"Number of dimensions specified does not match format.")]
      else []
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type.")]
    end
    ++
    data.err;
  
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    substExpr(
      data.substs,
      parseExpr(s"""
      ({
        double __tensor_data[] = {${data.asArray}};
        unsigned long __tensor_dims[] = {${data.dimArray}};
        
        struct tensor_${fmtNm}* _tensor = tensor_makeFilled_${fmtNm}(__tensor_dims, __tensor_data);
        _tensor;
      })
      """)
    );
    
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production tensor_array
top::Expr ::= type::TypeName dims::Expr
{
  local formatType::Type = type.typerep;
  
  local format::Name =
    case formatType of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;

  top.pp = ppConcat([
             text("build (tensor<${format.name}>) ("),
             dims.pp,
             text(")")
           ]);

  propagate substituted;
  
  local fmt::Decorated TensorFormatItem = format.tensorFormatItem;
  local dimens::Integer = fmt.dimens;
  
  local errors::[Message] =
    case formatType of
    | pointerType(_,
        tensorType(_, _, _)) ->
      format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type. Got ${showType(formatType)}")]
    end
    ++
    case dims.typerep of
    | arrayType(type, _, _, _) -> 
      if type.isIntegerType
      then []
      else [err(dims.location, s"Tensor must be built using an array of integer dimensions.")]
    | pointerType(_, type) ->
      if type.isIntegerType
      then []
      else [err(dims.location, s"Tensor must be built using an array of integer dimensions.")]      
    | _ -> [err(dims.location, s"Tensor must be built using an array of dimensions.")]
    end;
  
  local fmtNm::String = fmt.proceduralName;
  
  local dimType::Type =
    case dims.typerep of
    | arrayType(type, q, _, _) -> pointerType(q, type)
    | x -> x
    end;
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__dimens", dims) ::
        typedefSubstitution("__dim_type__", directTypeExpr(dimType)) ::
        [],
      parseExpr(s"""
      ({
        proto_typedef __dim_type__;
        __dim_type__ _dimens = __dimens;
        unsigned long* __tensor_arr = GC_malloc(sizeof(unsigned long) * ${toString(dimens)});
        for(unsigned long i = 0; i < ${toString(dimens)}; i++) {
          __tensor_arr[i] = _dimens[i];
        }
        struct tensor_${fmtNm}* _tensor = tensor_make_${fmtNm}(__tensor_arr);
        _tensor;
      })
      """)
    );
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production tensor_get
top::Expr ::= tensor::Expr index::[Expr]
{
  top.pp = ppConcat([
             text("value ("),
             tensor.pp,
             text(")("),
             ppImplode(text(", "), map((.pp), index)),
             text(")")
           ]);

  local errors::[Message] =
    case tensor.typerep of
    | pointerType(_,
        tensorType(_, _, _)
      ) -> []
    | _ -> [err(tensor.location, s"Tensor value expected a tensor (got ${showType(tensor.typerep)})")]
    end ++ 
    case tensor of
    | errorExpr(errs) -> errs
    | _ -> []
    end;
  
  propagate substituted;
  
  local fmtNm::String =
    case tensor.typerep of
    | pointerType(_, 
        tensorType(_,
          fmt,
          _
        )
      ) -> fmt.tensorFormatItem.proceduralName
    | _ -> "error"
    end;
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__tensor", tensor) ::
        generateSubstitutions(index, 0),
      parseExpr(s"""
      ({
        struct tensor_${fmtNm}* _tensor = __tensor;
        unsigned long __index[] = {${generateArray(index, 0)}};
        tensor_pack_${fmtNm}(_tensor);
        tensor_get_${fmtNm}(_tensor, __index);
      })
      """)
    );

  forwards to 
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production tensor_assign
top::Stmt ::= tensor::Expr index::[Expr] op::AssignmentOp val::Expr
{
  top.pp = ppConcat([
             text("value ("),
             tensor.pp,
             text(")("),
             ppImplode(text(", "), map((.pp), index)),
             text(")"),
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
          tensorType(_, _, _)
        ) -> []
      | _ -> [err(tensor.location, s"Tensor value expected a tensor (got ${showType(tensor.typerep)})")]
      end
    end;

  propagate substituted;
  top.functionDefs := [];
  
  local fmtNm::String =
    case tensor.typerep of
    | pointerType(_,
        tensorType(_,
          fmt,
          _
        )
      ) -> fmt.tensorFormatItem.proceduralName
    | _ -> "error"
    end;

  forwards to 
    if null(errors)
    then op.fwrd(fmtNm)(tensor, index, val)
    else warnStmt(errors);
    
}

abstract production tensor_compute
top::Stmt ::= base::Name index::[String] expr::TensorExpr
{
  expr.parenExpr = [];
  top.pp = ppConcat([
             text("tensor "),
             base.pp,
             text("("),
             ppImplode(text(", "), map(text(_), index)),
             text(") = "),
             expr.pp,
             text(";")
           ]);
  
  propagate substituted;
  top.functionDefs := [];
  expr.env = top.env;
  
  local baseValues::[ValueItem] =
    lookupValue(base.name, top.env);
  
  local errors::[Message] =
    case baseValues of
    | b::[] -> case b.typerep of
               | pointerType(_,
                   tensorType(_, fmt, _)
                 ) -> if !null(fmt.tensorFormatLookupCheck)
                      then fmt.tensorFormatLookupCheck
                      else
                      let f::Decorated TensorFormatItem =
                        fmt.tensorFormatItem
                      in
                      if listLength(index) != f.dimens
                      then [err(base.location, s"Tensor ${base.name} has ${toString(f.dimens)} dimensions, but accessed using ${toString(listLength(index))} index variables.")]
                      else []
                      end
               | _ -> [err(base.location, s"Tensor computation expected a tensor (got ${showType(b.typerep)})")]
               end
    | _ -> [err(base.location, s"Tensor computation expected a tensor")]
    end ++
    expr.errors;

  local duplicates::[Pair<Name String>] =
    tensorDuplicates(expr);
  local e::TensorExpr =
    replaceDuplicates(expr);

  forwards to 
  if !null(errors)
  then warnStmt(errors)
  else 
    seqStmt(
      foldl(
        \ stmt::Stmt p::Pair<Name String>
        -> let fmt::String =
             getFormat(p.fst, top.env).proceduralName
           in
           seqStmt(stmt, parseStmt(s"struct tensor_${fmt}* ${p.snd} = ${p.fst.name};"))
           end
        ,
        nullStmt(),
        duplicates
      ),
      codeGen(base, index, e, top.env, expr.location)
    ); 
}

abstract production orderof_type
top::Expr ::= tp::TypeName
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("orderof("),
      tp.pp,
      text(")")
    ]);

  local format::Name =
    case tp.typerep of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  local errors::[Message] =
    case tp.typerep of
    | pointerType(_,
        tensorType(_, _, _)) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, "orderof expected a tensor type (got ${showType(tp.typerep)})")]
    end;
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else mkIntConst(format.tensorFormatItem.dimens, top.location);
}

abstract production orderof_expr
top::Expr ::= tn::Expr
{
  top.pp = 
    ppConcat([
      text("orderof("),
      tn.pp,
      text(")")
    ]);
  propagate substituted;

  local format::Name =
    case tn.typerep of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  local errors::[Message] =
    case tn.typerep of
    | pointerType(_,
        tensorType(_, _, _)) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, "orderof expected a tensor type (got ${showType(tn.typerep)})")]
    end;
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else mkIntConst(format.tensorFormatItem.dimens, top.location);
}

abstract production dimenof
top::Expr ::= tn::Expr
{
  top.pp =
    ppConcat([
      text("dimenof("),
      tn.pp,
      text(")")
    ]);
  propagate substituted;

  local errors::[Message] =
    case tn.typerep of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt.tensorFormatLookupCheck
    | _ -> [err(top.location, "orderof expected a tensor type (got ${showType(tn.typerep)})")]
    end;
  
  local format::Name =
    case tn.typerep of
    | pointerType(_,
        tensorType(_, fmt, _)) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;
  
  local fmt::Decorated TensorFormatItem = format.tensorFormatItem;
  local fmtNm::String = fmt.proceduralName;
  local dimens::String = toString(fmt.dimens);
  
  local fwrd::Expr =
    substExpr(
      [declRefSubstitution("__tensor", tn)],
      parseExpr(s"""
      ({
        struct tensor_${fmtNm}* _tensor = __tensor;
        unsigned long* dims = GC_malloc(sizeof(unsigned long) * ${dimens});
        for(unsigned long i = 0; i < ${dimens}; i++) {
          dims[i] = _tensor->dims[i];
        }
        dims;
      })
      """)
    );
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}
