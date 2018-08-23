grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Here we declare the tensor_makeFilled function, used to create a tensor
   with pre-filled values. This uses tensor_make to construct the initial 
   tensor and then uses tensor_getPointer to insert every element through
   nested loops.
-}
function declMakeFilledFunction
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return
    maybeValueDecl(
      s"tensor_makeFilled_${fmtNm}",
       declMakeFilled(fmt)
    );
}

function declMakeFilled
Decl ::= fmt::TensorFormat
{
  local fmtNm::String = fmt.proceduralName;
  
  return 
    ableC_Decl {
      static void $name{s"tensor_makeFilled_${fmtNm}"}(struct $name{s"tensor_${fmtNm}"}* res, unsigned long* dims, double* data) {
        $name{s"tensor_make_${fmtNm}"}(res, dims);

        unsigned long index = 0;

        $Stmt{generateMakeFilledBody(fmt.dimensions, 0, fmtNm)}
      }
    };
}

function generateMakeFilledBody
Stmt ::= dims::Integer idx::Integer fmtNm::String
{
  local index::String = toString(idx);
  local next::String = toString(idx + 1);
  
  return
    if idx == dims
    then
      ableC_Stmt {
        unsigned long idx[] = $Initializer{generateIndexInitializer(dims)};
        *$name{s"tensor_getPointer_${fmtNm}"}(res, idx) = data[index];
        index++;
      }
    else
      ableC_Stmt {
        for(unsigned long $name{s"i${next}"} = 0; 
            $name{s"i${next}"} < dims[$intLiteralExpr{idx}]; 
            $name{s"i${next}"}++) {
          $Stmt{generateMakeFilledBody(dims, idx+1, fmtNm)}
        }
      };
}
