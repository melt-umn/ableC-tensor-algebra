grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:decls;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function declAll
Decl ::= fmt::TensorFormat
{
  return decls(
    consDecl(
      declStruct(fmt),
      consDecl(
        declInsertFunction(fmt),
        consDecl(
          declTensorGet(fmt),
          consDecl(
            declMakeFunction(fmt),
            consDecl(
              declPackFunction(fmt),
              consDecl(
                declMakeFilledFunction(fmt),
                nilDecl()
              )
            )
          )
        )
      )
    )
  );
}
