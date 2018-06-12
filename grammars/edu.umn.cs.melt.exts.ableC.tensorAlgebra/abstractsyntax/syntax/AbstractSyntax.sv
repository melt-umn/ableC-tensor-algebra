grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:syntax;

imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction:parsing;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;

imports silver:langutil;
imports silver:langutil:pp;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production format_without
top::Expr ::= types::[Integer]
{
  local errors::[Message] =
    case types of
    | [] -> [err(top.location, s"Cannot have zero dimensional tensor. Not supported")]
    | _ -> []
    end;
  
  propagate substituted;
  top.pp = ppConcat([
             text("format ({"),
             ppImplode(text(", "), map(\a::Integer -> text(if a == storeDense
                                                           then "dense"
                                                           else "sparse"), types)),
             text("})")
           ]);

  local order::[Integer] = defaultOrder(arrayLength(types));
  local fmt::TensorFormat = tensorFormat(types, order);
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    injectGlobalDeclsExpr(
      consDecl(declAll(fmt), nilDecl()),
      parseExpr(s"""
        ({
          struct tensor_format_s* fmt = GC_malloc(sizeof(struct tensor_format_s));
          fmt->make = tensor_make_${fmtNm};
          fmt->make_filled = tensor_makeFilled_${fmtNm};
          fmt->modify = tensor_modify_${fmtNm};
          fmt->get = tensor_get_${fmtNm};
          fmt->pack = tensor_pack_${fmtNm};
          fmt->order = ${toString(arrayLength(order))};
          fmt;
        })
      """),
      location = top.location
    );

  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production format_with
top::Expr ::= types::[Integer] order::[Integer]
{
  local dimens::Integer = arrayLength(order);

  local errors::[Message] =
    case types of
    | [] -> [err(top.location, s"Cannot have zero dimensional tensor. Not supported")]
    | _ -> []
    end ++
    if arrayLength(types) != dimens
    then [err(top.location, s"Tensor format must have same number of specified dimensions in type and order.")]
    else []
    ++
    if !checkOrder(order)
    then [err(top.location, s"Tensor format's order must contain each dimension once, represented as a number 0, 1, ..., d-1.")]
    else [];
  
  propagate substituted;
  top.pp = ppConcat([
             text("format ({"),
             ppImplode(text(", "), map(\a::Integer -> text(if a == storeDense
                                                           then "dense"
                                                           else "sparse"), types)),
             text("}, {"),
             ppImplode(text(", "), map(\a::Integer -> text(toString(a)), types)),
             text("})")
          ]);
  
  local fmt::TensorFormat = tensorFormat(types, order);
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    injectGlobalDeclsExpr(
      consDecl(declAll(fmt), nilDecl()),
      parseExpr(s"""
        ({
          struct tensor_format_s* fmt = GC_malloc(sizeof(struct tensor_format_s));
          fmt->make = tensor_make_${fmtNm};
          fmt->make_filled = tensor_makeFilled_${fmtNm};
          fmt->modify = tensor_modify_${fmtNm};
          fmt->get = tensor_get_${fmtNm};
          fmt->pack = tensor_pack_${fmtNm};
          fmt->order = ${toString(arrayLength(order))};
          fmt;
        })
      """),
      location = top.location
    );
  
  forwards to
  if !null(errors)
  then errorExpr(errors, location=top.location)
  else fwrd;
}

abstract production tensor_empty
top::Expr ::= dims::[Expr] format::Expr
{
  top.pp = ppConcat([
             text("build tensor ({"),
             ppImplode(text(", "), map((.pp), dims)),
             text("}, "),
             format.pp,
             text(")")
           ]);
  
  propagate substituted;
  
  local errors::[Message] = 
    case format.typerep of
    | pointerType(_, 
        tagType(_, 
          refIdTagType(
            structSEU(), 
            "tensor_format_s", 
            "edu:umn:cs:melt:exts:ableC:tensorAlgebra:format"
          )
        )
      ) -> []
    | _ -> [err(format.location, s"Build tensor expected a tensor format (got ${showType(format.typerep)})")]
    end ++
    case format of
    | errorExpr(errs) -> errs
    | _ -> []
    end;
    
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__format", format) ::
        generateSubstitutions(dims, 0),
      parseExpr(s"""
      ({
        struct tensor_format_s* _format = __format;
        if(${toString(arrayLength(dims))} != _format->order) {
          fprintf(stderr, "Dimensions given to build tensor does not match the format (line ${toString(top.location.line)})");
          exit(1);
        }
        unsigned long __tensor_arr[] = {${generateArray(dims, 0)}};
        struct tensor_s* _tensor = _format->make(__tensor_arr);
        _tensor->make = _format->make;
        _tensor->make_filled = _format->make_filled;
        _tensor->modify = _format->modify;
        _tensor->get = _format->get;
        _tensor->pack = _format->pack;
        _tensor->order = _format->order;
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
top::Expr ::= data::Tensor format::Expr
{
  top.pp = ppConcat([
             text("build tensor ("),
             data.pp,
             text(", "),
             format.pp,
             text(")")
           ]);

  propagate substituted;
  
  data.pos = "";
  data.env = top.env;
  
  local errors::[Message] =
    case format.typerep of
    | pointerType(_,
        tagType(_,
          refIdTagType(
            structSEU(),
            "tensor_format_s",
            "edu:umn:cs:melt:exts:ableC:tensorAlgebra:format"
          )
        )
      ) -> []
    | _ -> [err(format.location, s"Build tensor expected a tensor format (got ${showType(format.typerep)})")]
    end ++ data.err ++
    case format of
    | errorExpr(errs) -> errs
    | _ -> []
    end;
  
  local fwrd::Expr =
    substExpr(
      declRefSubstitution(s"__format", format) ::
        data.substs,
      parseExpr(s"""
      ({
        struct tensor_format_s* _format = __format;
        if(${toString(data.dimensions)} != _format->order) {
          fprintf(stderr, "Dimensions given to build tensor does not match the format (line ${toString(top.location.line)})");
          exit(1);
        }
        double __tensor_data[] = {${data.asArray}};
        unsigned long __tensor_dims[] = {${data.dimArray}};
        
        struct tensor_s* _tensor = _format->make_filled(__tensor_dims, __tensor_data);
        _tensor->make = _format->make;
        _tensor->make_filled = _format->make_filled;
        _tensor->modify = _format->modify;
        _tensor->get = _format->get;
        _tensor->pack = _format->pack;
        _tensor->order = _format->order;
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
top::Expr ::= tensor::Name index::[Expr]
{
  top.pp = ppConcat([
             text("value "),
             tensor.pp,
             text("("),
             ppImplode(text(", "), map((.pp), index)),
             text(")")
           ]);
  
  propagate substituted;
  
  local fwrd::Expr =
    substExpr(
      generateSubstitutions(index, 0),
      parseExpr(s"""
      ({
        unsigned long __index[] = {${generateArray(index, 0)}};
        ${tensor.name}->pack(${tensor.name});
        ${tensor.name}->get(${tensor.name}, __index);
      })
      """)
    );

  forwards to fwrd;
}
