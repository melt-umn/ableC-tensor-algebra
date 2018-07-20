grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production build_empty
top::Expr ::= type::TypeName dims::[Expr]
{
  local formatType::Type = type.typerep;

  local format::Name =
    case formatType of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("error", location=top.location)
    end;
  format.env = top.env;

  top.pp =
    ppConcat([
      text("build tensor<${format.name}> ({"),
      ppImplode(text(", "), map((.pp), dims)),
      text("})")
    ]);
  
  propagate substituted;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local dimens::Integer = fmt.dimensions;
  
  local lErrors::[Message] =
    case formatType of
    | tensorType(_, _, _) -> 
        format.tensorFormatLookupCheck
        ++
        if dimens > 0 && dimens != listLength(dims)
        then [err(top.location, "Number of dimensions specified does not match format.")]
        else []
    | _ -> [err(top.location, "Tensor cannot be built using a non-tensor type.")]
    end
    ++
    flatMap(
      \ e::Expr
      -> let ex::Decorated Expr = decorate e with {env=top.env;returnType=nothing();}
         in
         if null(ex.errors)
         then if ex.typerep.isIntegerType
              then []
              else [err(ex.location, s"Expected an integer type, got a ${showType(ex.typerep)}")]
         else ex.errors
         end,
      dims
    );
  
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    substExpr(
      generateSubstitutions(dims, 0),
      parseExpr(s"""
      ({
        unsigned long __tensor_arr[] = {${generateArray(dims, 0)}};
        struct tensor_${fmtNm} _tensor;
        tensor_make_${fmtNm}(&_tensor, __tensor_arr);
        _tensor;
      })
      """)
    );

  forwards to mkErrorCheck(lErrors, fwrd);
}

abstract production build_data
top::Expr ::= type::TypeName data::TensorConstant
{
  local formatType::Type = type.typerep;
  
  local format::Name =
    case formatType of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;
  
  top.pp = ppConcat([
             text("build (tensor<${format.name}>) ("),
             data.pp,
             text(")")
           ]);

  propagate substituted;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local dimens::Integer = fmt.dimensions;
  
  data.tensor_pos = "";
  data.env = top.env;
  
  local lErrors::[Message] =
    case formatType of
    | tensorType(_, _, _) -> 
       format.tensorFormatLookupCheck
       ++
       if null(format.tensorFormatLookupCheck) && dimens != data.tensor_dims
       then [err(top.location, "Number of dimensions specified does not match format.")]
       else []
    | _ -> [err(top.location, "Tensor cannot be built using a non-tensor type. Got ${showType(formatType)}")]
    end
    ++
    data.errors;
  
  local fmtNm::String = fmt.proceduralName;
  
  local fwrd::Expr =
    substExpr(
      data.tensor_substs,
      parseExpr(s"""
      ({
        double __tensor_data[] = {${data.tensor_asArray}};
        unsigned long __tensor_dims[] = {${data.tensor_dimArray}};
        
        struct tensor_${fmtNm} _tensor;
        tensor_makeFilled_${fmtNm}(&_tensor, __tensor_dims, __tensor_data);
        _tensor;
      })
      """)
    );
  
  forwards to mkErrorCheck(lErrors, fwrd);
}

abstract production build
top::Expr ::= type::TypeName args::[Expr]
{
  local dims::Expr = head(args);
  local formatType::Type = type.typerep;
  
  local format::Name = 
    case formatType of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;
  
  top.pp = ppConcat([
             text("build (tensor<${format.name}>) ("),
             dims.pp,
             text(")")
           ]);

  propagate substituted;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local dimens::Integer = fmt.dimensions;

  dims.env = top.env;
  dims.returnType = nothing();  

  local lErrors::[Message] = 
    case formatType of
    | tensorType(_, _, _) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type. Got ${showType(formatType)}")]
    end
    ++
    case args of
    | [] -> [err(dims.location, "Tensor must be built using one expression, not zero.")]
    | _::[] ->
       case dims.typerep of
       | arrayType(type, _, _, _) ->
          if type.isIntegerType
          then []
          else [err(dims.location, s"Tensor must be built using an array of integer dimensions. Got ${showType(dims.typerep)}.")]
       | pointerType(_, type) ->
          if type.isIntegerType
          then []
          else [err(dims.location, s"Tensor must be built using a pointer of integer type. Got ${showType(dims.typerep)}.")]
       end
    | _::_ -> [err(dims.location, "Tensor must be built using one expression, not multiple.")]
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
        struct tensor_${fmtNm} _tensor;
        tensor_make_${fmtNm}(&_tensor, __tensor_arr);
        _tensor;
      })
      """)
    );

  forwards to mkErrorCheck(lErrors, fwrd);
}
