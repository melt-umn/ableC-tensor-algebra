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

  local tensorAcc :: Boolean =
    case bounds.typerep of
    | extType(_, tensorType(_)) -> true
    | _ -> false
    end;

  local justTensor :: Boolean =
    case bounds.typerep of
    | extType(_, tensorAccType()) -> true
    | _ -> false
    end;

  local tensor :: TensorExpr =
    bounds.tensorExp;
  tensor.fmts = tm:empty(compareString);

  local fmt :: TensorFormat =
    if tensorAcc
    then getTensorFormat(tensor, tm:empty(compareString))
    else 
      case bounds.typerep of
      | extType(_, tensorType(f)) -> new(f.tensorFormat)
      end;

  local access :: [Either<Expr String>] =
    if tensorAcc
    then tensor.iterAccess
    else
      map(
        \ i::Integer ->
          right(s"__v${toString(i)}")
        ,
        makeList(integerCompare, inc, 0, fmt.dimensions)
      );

  local stmts :: [(Stmt ::= Stmt)] =
    map(
      \ e::Pair<Either<Expr String> Pair<Integer Pair<Integer Integer>>> ->
        let indexEmit :: Stmt =
          if justTensor
          then
            ableC_Stmt {
              index[$intLiteralExpr{e.snd.snd.fst}] = $name{e.fst.fromRight};
            }
          else nullStmt()
        in
        if e.snd.snd.snd == storeDense 
        then
          if e.fst.isLeft
          then
            \ bd::Stmt ->
              if e.snd.fst == 0
              then -- First dimension, dense, Expr
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
                  $Stmt{bd}
                }
              else -- Not first dimension, dense, Expr
                ableC_Stmt {
                  unsigned long $name{s"p${toString(e.snd.fst+1)}"};
                  {
                    unsigned long temp = $Expr{e.fst.fromLeft};
                    if(temp >= $name{s"size_${toString(e.snd.fst+1)}"}) {
                      fprintf(stderr, 
                        $stringLiteralExpr{let loc::Location = e.fst.fromLeft.location in s"Size out of bounds in foreach loop. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
                      exit(1);
                    }
                    $name{s"p${toString(e.snd.fst+1)}"} = ($name{s"p${toString(e.snd.fst)}"} * $name{s"size_${toString(e.snd.fst+1)}"}) + temp;
                  }
                  $Stmt{bd}
                }
          else
            \ bd::Stmt ->
              if e.snd.fst == 0
              then -- First dimension, dense, indexvar
                ableC_Stmt {
                  for(unsigned long $name{e.fst.fromRight} = 0; $name{e.fst.fromRight} < $name{s"size_${toString(e.snd.fst+1)}"}; $name{e.fst.fromRight}++) {
                    unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{e.fst.fromRight};
                    $Stmt{indexEmit}
                    $Stmt{bd}
                  }
                }
              else -- Not first dimension, dense, indexvar
                ableC_Stmt {
                  for(unsigned long $name{e.fst.fromRight} = 0; $name{e.fst.fromRight} < $name{s"size_${toString(e.snd.fst+1)}"}; $name{e.fst.fromRight}++) {
                    unsigned long $name{s"p${toString(e.snd.fst+1)}"} = ($name{s"p${toString(e.snd.fst)}"} * $name{s"size_${toString(e.snd.fst+1)}"}) + $name{e.fst.fromRight};
                    $Stmt{indexEmit}
                    $Stmt{bd}
                  }
                }
        else
          if e.fst.isLeft
          then
            \ bd::Stmt ->
              if e.snd.fst == 0
              then -- First dimension, sparse, Expr
                ableC_Stmt {
                  unsigned long target = $Expr{e.fst.fromLeft};
                  if(target >= $name{s"size_${toString(e.snd.fst+1)}"}) {
                    fprintf(stderr, 
                      $stringLiteralExpr{let loc::Location = e.fst.fromLeft.location in s"Size out of bounds in foreach loop. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
                    exit(1);
                  }
                  for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                    if($name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}] == target) {
                      $Stmt{bd}
                      break;
                    }
                  }
                }
              else -- Not first dimension, sparse, Expr
                ableC_Stmt {
                  unsigned long target = $Expr{e.fst.fromLeft};
                  if(target >= $name{s"size_${toString(e.snd.fst+1)}"}) {
                    fprintf(stderr, 
                      $stringLiteralExpr{let loc::Location = e.fst.fromLeft.location in s"Size out of bounds in foreach loop. (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})\n" end});
                    exit(1);
                  }
                  for(unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}]; $name{s"p${toString(e.snd.fst+1)}"} < $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}+1]; $name{s"p${toString(e.snd.fst+1)}"}++) {
                    if($name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}] == target){
                      $Stmt{bd}
                      break;
                    }
                  }
                }
          else
            \ bd::Stmt ->
              if e.snd.fst == 0
              then -- First dimension, sparse, indexvar
                ableC_Stmt {
                  for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                    unsigned long $name{e.fst.fromRight} = $name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}];
                    $Stmt{indexEmit}
                    $Stmt{bd}
                  }
                }
              else -- Not first dimension, sparse, indexvar
                ableC_Stmt {
                  for(unsigned long $name{s"p${toString(e.snd.fst+1)}"} = $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}]; $name{s"p${toString(e.snd.fst+1)}"} < $name{s"pos_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst)}"}+1]; $name{s"p${toString(e.snd.fst+1)}"}++) {
                    unsigned long $name{e.fst.fromRight} = $name{s"idx_${toString(e.snd.fst+1)}"}[$name{s"p${toString(e.snd.fst+1)}"}];
                    $Stmt{indexEmit}
                    $Stmt{bd}
                  }
                }
        end
      ,
      zipWith(
        pair,
        access, -- expr or indexvar 
        fmt.storage
      )
    );

  local loops :: Stmt =
    foldr(
      \ fnc::(Stmt ::= Stmt) s::Stmt ->
        fnc(s)
      ,
      ableC_Stmt {
        double $name{var.name} = __data[$name{s"p${toString(fmt.dimensions)}"}];
        $Stmt{body}
      },
      stmts
    );

  local init :: [Stmt] =
    if tensorAcc
    then tensorVals(tensor, fmt, top.env)
    else 
      case bounds of
      | declRefExpr(_) -> nullStmt()
      | _ -> 
        ableC_Stmt {
          struct $name{s"tensor_${fmt.proceduralName}"} __t = $Expr{bounds};
        }
      end
      ::
      ableC_Stmt {
        $name{s"tensor_pack_${fmt.proceduralName}"}(&$name{nm});
        pthread_rwlock_rdlock(&($name{nm}.lock));
        double* __data = $name{nm}.data;
        unsigned long index[$intLiteralExpr{fmt.dimensions}];
      }
      ::
      map(
        \ p::Pair<Integer Pair<Integer Integer>> ->
          if p.snd.snd == storeDense
          then
            ableC_Stmt {
              unsigned long $name{s"size_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0][0];
            }
          else
            ableC_Stmt {
              unsigned long* $name{s"pos_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][0];
              unsigned long* $name{s"idx_${toString(p.fst+1)}"} = $name{nm}.indices[$intLiteralExpr{p.snd.fst}][1];
              unsigned long $name{s"size_${toString(p.fst+1)}"} = $name{nm}.dims[$intLiteralExpr{p.snd.fst}];
            }
        ,
        fmt.storage
      );

  local nm :: String =
    if tensorAcc
    then getTensorName(tensor)
    else 
      case bounds of
      | declRefExpr(nm) -> nm.name
      | _ -> "__t"
      end;

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

  local newEnv :: Decorated Env =
    addEnv(
      valueDef(var.name,
        builtinValueItem(builtinType(nilQualifier(), realType(doubleType()))))
      ::
      if tensorAcc
      then
        maybeMap(
          \ mb::Either<Expr String> ->
            if mb.isLeft
            then nothing()
            else
              just(
                valueDef(mb.fromRight,
                  builtinValueItem(
                    builtinType(nilQualifier(), unsignedType(longType()))))
              )
          ,
          access
        )
      else
        valueDef("index",
          builtinValueItem(
            arrayType(
              builtinType(nilQualifier(), unsignedType(longType())),
              nilQualifier(),
              normalArraySize(),
              constantArrayType(fmt.dimensions)
            )
          )
        ) :: [],
      top.env
    );
  body.env = newEnv;

  local lErrors :: [Message] =
    checkTensorHeader(var.location, top.env)
    ++
    bounds.errors
    ++
    body.errors
    ++
    if tensorAcc
    then []
    else if justTensor
    then 
      case bounds.typerep of
      | extType(_, tensorType(_)) -> []
      | _ -> [err(bounds.location, s"Tensor for-each loop expected a tensor type. Instead got ${showType(bounds.typerep)}.")]
      end
    else [err(bounds.location, s"Tensor for-each loop expected a tensor access expression. Instead got ${showType(bounds.typerep)}.")];

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
    | tensorAccess(e, _, _) ->
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
      double* __data = $name{nm}.data;
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
          } ::
          ableC_Stmt {
            unsigned long $name{s"size_${toString(p.fst+1)}"} = $name{nm}.dims[$intLiteralExpr{p.snd.fst}];
          }
          :: []
      ,
      fmt.storage
    );
}
