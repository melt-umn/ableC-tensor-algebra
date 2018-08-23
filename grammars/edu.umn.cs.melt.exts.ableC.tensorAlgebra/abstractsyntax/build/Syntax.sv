grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Production to build an empty tensor. This process is relatively
   simple, we take the provided dimensions and create a tensor of
   the appropriate type with those dimensions and minimally viable
   data in it. This means, though, that all other functions can be
   called without packing and work as desired.
-}
abstract production build_empty
top::Expr ::= type::TypeName dims::[Expr]
{
  local format::Name =
    case type.typerep of
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
    checkTensorHeader(top.location, top.env)
    ++
    case type.typerep of
    | tensorType(_, _, _) -> 
        format.tensorFormatLookupCheck
        ++
        if dimens > 0 && dimens != listLength(dims)
        then [err(top.location, "Number of dimensions specified does not match format.")]
        else []
    | _ -> [err(top.location, "Tensor cannot be built using a non-tensor type. Got ${showType(type.typerep)}")]
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
  
  -- The array initializer, like
  -- { a, b, c }
  local dimInit :: Initializer =
    objectInitializer(
      foldr(
        \ e::Expr lst::InitList ->
          consInit(
            positionalInit(
              exprInitializer(e)
            )
            ,
            lst
          )
        ,
        nilInit(),
        dims
      )
    );

  local fwrd::Expr =
    ableC_Expr {
      ({
        unsigned long __tensor_arr[$intLiteralExpr{fmt.dimensions}] = $Initializer{dimInit};
        struct $name{s"tensor_${fmtNm}"} _tensor = {0};
        $name{s"tensor_make_${fmtNm}"}(&_tensor, __tensor_arr);
        _tensor;
      })
    };

  forwards to mkErrorCheck(lErrors, fwrd);
}

{- For building a tensor when all the data in the tensor is provided.
   Uses TensorConstant to get dimensions array and data array.
-}
abstract production build_data
top::Expr ::= type::TypeName data::TensorConstant
{
  local format::Name =
    case type.typerep of
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
  
  data.env = top.env;
  
  local lErrors::[Message] =
    checkTensorHeader(top.location, top.env)
    ++
    case type.typerep of
    | tensorType(_, _, _) -> 
       format.tensorFormatLookupCheck
       ++
       if null(format.tensorFormatLookupCheck) && dimens != data.tensor_dims
       then [err(top.location, "Number of dimensions specified does not match format.")]
       else []
    | _ -> [err(top.location, "Tensor cannot be built using a non-tensor type. Got ${showType(type.typerep)}")]
    end
    ++
    data.errors;
  
  local fmtNm::String = fmt.proceduralName;
  
  -- by setting __tensor_location, error messages from tensor_makeFilled
  -- will have the file name and line / column that resulted in the call
  local fwrd::Expr =
    ableC_Expr {
      ({
        double __tensor_data[] = $Initializer{data.tensor_asExpr};
        unsigned long __tensor_dims[] = $Initializer{data.tensor_dimExpr};

        struct $name{s"tensor_${fmtNm}"} _tensor = {0};
        __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
        $name{s"tensor_makeFilled_${fmtNm}"}(&_tensor, __tensor_dims, __tensor_data);
        _tensor;
      })
    };
  
  forwards to mkErrorCheck(lErrors, fwrd);
}

{- Production for building a tensor from a list of expressions,
   or in reality a single expression. Multiple expressions are
   treated as an error. A single array / pointer is expected,
   containing an integer type with the desired dimensions of the
   tensor.
-}
abstract production buildTensorExpr
top::Expr ::= type::TypeName args::[Expr]
{
  local dims::Expr = head(args);
  
  local format::Name = 
    case type.typerep of
    | tensorType(_, fmt, _) -> fmt
    | _ -> name("__error__", location=top.location)
    end;
  format.env = top.env;
  
  top.pp = ppConcat(
             text("build (tensor<${format.name}>) (") ::
             map((.pp), args) ++
             [text(")")]
           );

  propagate substituted;
  
  local fmt::Decorated TensorFormat = format.tensorFormat;
  local dimens::Integer = fmt.dimensions;

  dims.env = top.env;
  dims.returnType = nothing();  

  local lErrors::[Message] = 
    checkTensorHeader(top.location, top.env)
    ++
    case type.typerep of
    | tensorType(_, _, _) -> format.tensorFormatLookupCheck
    | _ -> [err(top.location, s"Tensor cannot be built using a non-tensor type. Got ${showType(type.typerep)}")]
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
      | _ -> [err(dims.location, s"Tensor must be built using an array of integer types. Got ${showType(dims.typerep)}.")]
      end
    | _::_ -> [err(dims.location, "Tensor must be built using one expression, not multiple.")]
    end;
  
  
  local fmtNm::String = fmt.proceduralName;
 
  -- The elements of the list are copied from the input array into
  -- another array to ensure the correct type of the array.
  local fwrd::Expr =
    ableC_Expr {
      ({
        $BaseTypeExpr{dims.typerep.baseTypeExpr}* _dimens = $Expr{dims};
        unsigned long __tensor_arr[$intLiteralExpr{dimens}];
        for(unsigned long i = 0; i < $intLiteralExpr{dimens}; i++) {
          __tensor_arr[i] = _dimens[i];
        }
        struct $name{s"tensor_${fmtNm}"} _tensor = {0};
        $name{s"tensor_make_${fmtNm}"}(&_tensor, __tensor_arr);
        _tensor;
      })
    };

  forwards to mkErrorCheck(lErrors, fwrd);
}
