grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:indexvar;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production indexvar
top::Decl ::= nms::[Name]
{
  local errors::[Message] =
    checkTensorHeader(head(nms).location, top.env);

  propagate substituted;
  top.pp = ppConcat([
             text("indexvar "),
             ppImplode(
               text(", "),
               map(
                 \ n::Name -> text(n.name),
                 nms)
             ),
             text(";")
           ]);

  local fwrd::Decl =
    variableDecls(
      nilStorageClass(),
      nilAttribute(),
      indexvarTypeExpr(nilQualifier()),
      foldl(
        \ d::Declarators nm::Name
        -> consDeclarator(
             declarator(nm, baseTypeExpr(), nilAttribute(), nothingInitializer()), 
             d
           )
        ,
        nilDeclarator(),
        nms
      )
    );
  
  forwards to
  if !null(errors)
  then warnDecl(errors)
  else fwrd;
}
