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
              if e.snd.fst == 0
              then
                ableC_Stmt {
                  unsigned long $name{s"p${toString(e.snd.fst+1)}"};
                  {
                    unsigned long temp = $Expr{e.fst.fromLeft};
                    if(temp >= $name{s"size_${toString(e.snd.fst+1)}"}) {
                      fprintf(stderr, 
                        $stringLiteralExpr{let loc::Location = e.fst.fromLeft.location in s"Size out of bounds in foreach loop. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
                      exit(1);
                    }
                    $name{s"p${toString(e.snd.fst+1)}"} = temp;
                  }
                  $Stmt{bd};
                }
              else
                ableC_Stmt {
                  unsigned long $name{s"p${toString(e.snd.fst+1)}"};
                  {
                    unsigned long temp = $Expr{e.fst.fromLeft};
                    if(temp >= $name{s"size_${toString(e.snd.fst+1)}"}) {
                      fprintf(stderr, 
                        $stringLiteralExpr{let loc::Location = e.fst.fromLeft.location in "Size out of bounds in foreach loop. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
                      exit(1);
                    }
                    $name{s"p${toString(e.snd.fst+1)}"} = ($name{s"p${toString(e.snd.fst)}"} * $name{s"size_${toString(e.snd.fst+1)}"}) + temp;
                  }
                  $Stmt{bd};
                }
          else
            \ bd::Stmt ->
              if e.snd.fst == 0
              then
                ableC_Stmt {
                  for(unsigned long $name{e.fst.fromRight} = 0; $name{e.fst.fromRight} < $name{s"size_${toString(e.snd.fst+1)}"}; $name{e.fst.fromRight}++) {
                    unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{e.fst.fromRight};
                    $Stmt{bd};
                  }
                }
              else 
                ableC_Stmt {
                  for(unsigned long $name{e.fst.fromRight} = 0; $name{e.fst.fromRight} < $name{s"size_${toString(e.snd.fst+1)}"}; $name{e.fst.fromRight}++) {
                    unsigned long $name{s"p${toString(e.snd.fst+1)}"} = ($name{s"p${toString(e.snd.fst)}"} * $name{s"size_${toString(e.snd.fst+1)}"}) + $name{e.fst.fromRight};
                    $Stmt{bd};
                  }
                }
        else
          if e.fst.isLeft
          then
            \ bd::Stmt ->
              if e.snd.fst == 0
              then
                ableC_Stmt {
                  unsigned long target = $Expr{e.fst.fromLeft};
                  for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                    if($name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}] == target) {
                      $Stmt{bd};
                      break;
                    }
                  }
                }
              else
                ableC_Stmt {
                  unsigned long target = $Expr{e.fst.fromLeft};
                  for(unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}]; $name{s"p${toString(e.snd.fst+1)}"} < $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}+1]; $name{s"p${toString(e.snd.fst+1)}"}++) {
                    if($name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}] == target){
                      $Stmt{bd};
                      break;
                    }
                  }
                }
          else
            \ bd::Stmt ->
              if e.snd.fst == 0
              then
                ableC_Stmt {
                  for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                    unsigned long $name{e.fst.fromRight} = $name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}];
                    $Stmt{bd};
                  }
                }
              else
                ableC_Stmt {
                  for(unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}]; $name{s"p${toString(e.snd.fst+1)}"} < $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}+1]; $name{s"p${toString(e.snd.fst+1)}"}++) {
                    unsigned long $name{e.fst.fromRight} = $name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}];
                    $Stmt{bd};
                  }
                }
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
        ableC_Stmt {
          double $name{var.name} = data[$name{s"p${toString(fmt.dimensions)}"}];
        },
        body
      ),
      stmts
    );

  local init :: [Stmt] =
    tensorVals(tensor, fmt, top.env);

  local nm :: String =
    getTensorName(tensor);

  local fwrd :: Stmt =
    compoundStmt(
      seqStmt(
        foldr(
          \ nw::Stmt inn::Stmt ->
            seqStmt(nw, inn)
          ,
          loops,
          init
        ),
        ableC_Stmt {
          pthread_rwlock_unlock(&($name{nm}.lock));
        }
      )
    );

  fwrd.env = top.env;
  fwrd.returnType = top.returnType;

  local sErrors :: [Message] =
    checkTensorHeader(var.location, top.env)
    ++
    bounds.errors;

  local lErrors :: [Message] =
    sErrors
    ++
    if null(sErrors)
    then fwrd.errors
    else [];

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
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
        ableC_Stmt {
          struct $name{s"tensor_${fmt.proceduralName}"} $name{s"_tensor_${toString(ex.location.line)}_${toString(e.location.column)}"} = $Expr{e};
        }
      end
    | _ -> nullStmt()
    end
    ::
    ableC_Stmt {
      $name{s"tensor_pack_${fmt.proceduralName}"}(&$name{nm});
    }
    ::
    ableC_Stmt {
      pthread_rwlock_rdlock(&($name{nm}.lock));
    }
    ::
    ableC_Stmt {
      double* data = $name{nm}.data;
    }
    ::
    flatMap(
      \ p::Pair<Integer Pair<Integer Integer>> ->
        if p.snd.snd == storeDense
        then
          ableC_Stmt {
            unsigned long $name{s"size_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0][0];
          } :: []
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
