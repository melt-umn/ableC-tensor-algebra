grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:foreach;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorForEach
top::Stmt ::= var::Name bounds::Expr body::Stmt
{
  propagate substituted;

  top.functionDefs := [];
  top.pp =
    ppConcat([
      text("foreach ("),
      text("double "),
      var.pp,
      text(" : "),
      bounds.pp,
      text(")\n"),
      body.pp
   ]);

  bounds.env = top.env;

  local tensor :: TensorExpr =
    bounds.tensorExp;
  tensor.fmts = tm:empty(compareString);

  local access :: [Either<Expr String>] =
    tensor.iterAccess;

  local fmt :: TensorFormat =
    getTensorFormat(tensor, tm:empty(compareString));

  local stmts :: [(Stmt ::= Stmt)] =
    map(
      \ e::Pair<Either<Expr String> Pair<Integer Integer>> ->
        if e.snd.snd == storeDense
        then
          if e.fst.isLeft
          then
            \ bd::Stmt ->
              substStmt(
                declRefSubstitution("__expr", e.fst.fromLeft) ::
                stmtSubstitution("__stmt", bd) :: [],
                parseStmt(
                  -- The following error check should be performed.
                  --s"if(__expr >= size_${toString(e.snd.fst+1)}) { fprintf(stderr, \"Size out of bounds in foreach loop.\"); exit(1); }" ++
                  (
                  if e.snd.fst == 0
                  then s"unsigned long p${toString(e.snd.fst+1)} = __expr;"
                  else s"unsigned long p${toString(e.snd.fst+1)} = (p${toString(e.snd.fst)} * size_${toString(e.snd.fst+1)}) + __expr;"
                  )
                  ++
                  "__stmt;"
                )
              )
          else
            \ bd::Stmt ->
              substStmt(
                stmtSubstitution("__stmt", bd) :: [],
                parseStmt(
                s"for(unsigned long ${e.fst.fromRight} = 0; ${e.fst.fromRight} < size_${toString(e.snd.fst+1)}; ${e.fst.fromRight}++) {"
                ++
                (
                if e.snd.fst == 0
                then s"unsigned long p${toString(e.snd.fst+1)} = ${e.fst.fromRight};"
                else s"unsigned long p${toString(e.snd.fst+1)} = (p${toString(e.snd.fst)} * size_${toString(e.snd.fst+1)}) + ${e.fst.fromRight};"
                )
                ++
                "__stmt;"
                ++
                "}"
                )
              )
        else
          if e.fst.isLeft
          then
            \ bd::Stmt ->
              substStmt(
                declRefSubstitution("__expr", e.fst.fromLeft) ::
                stmtSubstitution("__stmt", bd) :: [],
                parseStmt(
                (
                if e.snd.fst == 0
                then s"for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {"
                else s"for(unsigned long p${toString(e.snd.fst+1)} = pos_${toString(e.snd.fst+1)}[p${toString(e.snd.fst)}]; p${toString(e.snd.fst+1)} < pos_${toString(e.snd.fst+1)}[p${toString(e.snd.fst)}+1]; p${toString(e.snd.fst+1)}++) {"
                )
                ++
                s"if(idx_${toString(e.snd.fst+1)}[p${toString(e.snd.fst+1)}] == __expr) {"
                ++
                "__stmt; break;"
                ++
                "} }"
                )
              )
          else
            \ bd::Stmt ->
              substStmt(
                stmtSubstitution("__stmt", bd) :: [],
                parseStmt(
                (
                if e.snd.fst == 0
                then s"for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {"
                else s"for(unsigned long p${toString(e.snd.fst+1)} = pos_${toString(e.snd.fst+1)}[p${toString(e.snd.fst)}]; p${toString(e.snd.fst+1)} < pos_${toString(e.snd.fst+1)}[p${toString(e.snd.fst)}+1]; p${toString(e.snd.fst+1)}++) {"
                )
                ++
                s"unsigned long ${e.fst.fromRight} = idx_${toString(e.snd.fst+1)}[p${toString(e.snd.fst+1)}];"
                ++
                "__stmt;"
                ++
                "}"
                )
              )
      ,
      zipWith(
        pair,
        access,
        map(
          \ p::Pair<Integer Pair<Integer Integer>> ->
            pair(p.fst, p.snd.snd)
          ,
          fmt.storage
        )
      )
    );

  local loops :: Stmt =
    foldr(
      \ fnc::(Stmt ::= Stmt) s::Stmt ->
        fnc(s)
      ,
      seqStmt(
        parseStmt(s"double ${var.name} = data[p${toString(fmt.dimensions)}];"),
        body
      ),
      stmts
    );

  local init :: [Stmt] =
    tensorVals(tensor, fmt, top.env);

  local fwrd :: Stmt =
    compoundStmt(
      foldr(
        \ nw::Stmt inn::Stmt ->
          seqStmt(nw, inn)
        ,
        loops,
        init
      )
    );

  fwrd.env = top.env;
  fwrd.returnType = top.returnType;

  forwards to
    if !null(fwrd.errors)
    then warnStmt(fwrd.errors)
    else fwrd;
}

function tensorVals
[Stmt] ::= ex::TensorExpr fmt::TensorFormat env::Decorated Env
{
  local nm::String = getTensorName(ex);

  return
    case ex of
    | tensorAccess(_, e, _, _) ->
      case decorate e with {env=env; returnType=nothing();} of
      | declRefExpr(name(_)) -> nullStmt()
      | _ -> 
        substStmt(
          [declRefSubstitution("__expr", e)],
          parseStmt(s"struct tensor_${fmt.proceduralName} _tensor_${toString(ex.location.line)}_${toString(ex.location.column)} = __expr;")
        )
      end
    | _ -> nullStmt()
    end
    ::
    parseStmt(s"tensor_pack_${fmt.proceduralName}(&${nm});")
    ::
    parseStmt(s"double* data = ${nm}.data;")
    ::
    flatMap(
      \ p::Pair<Integer Pair<Integer Integer>> ->
        if p.snd.snd == storeDense
        then
          [parseStmt(s"unsigned long size_${toString(p.fst+1)} = ${nm}.indices[${toString(p.snd.fst)}][0][0];")]
        else
          ableC_Stmt {
            unsigned long* $name{s"pos_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0];
          } ::
          ableC_Stmt { 
            unsigned long* $name{s"idx_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][1];
          }
          :: []
      ,
      fmt.storage
    );
}
