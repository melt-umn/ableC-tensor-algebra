grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:indexvar;

abstract production indexvar
top::Decl ::= nms::[Name]
{
  local errors::[Message] =
    flatMap(
      \ n::Name -> (decorate n with {env=top.env;}).indexVarRedeclarationCheck,
      nms
    );
  
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

  local vars::[IndexVar] =
    map(
      \ n::Name -> indexVar(n, n.location),
      nms
    );
  
  local fwrd::Decl =
    decls(
      foldl(
        \ d::Decls v::IndexVar
        -> 
        consDecl(
          defsDecl([indexVarDef(v.variable, v)]),
          d
        )
        ,
        nilDecl(),
        vars
      )
    );
  
  forwards to
  if !null(errors)
  then warnDecl(errors)
  else fwrd;
}
