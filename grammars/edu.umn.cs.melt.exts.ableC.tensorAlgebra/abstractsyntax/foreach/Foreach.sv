grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:foreach;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorForEach
top::Stmt ::= var::Name bounds::Expr body::Stmt
{

  top.functionDefs := [];
  top.labelDefs := [];
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

  bounds.controlStmtContext = top.controlStmtContext;

  bounds.env = top.env;

  local tensorAcc :: Boolean =
    case bounds.typerep of
    | extType(_, tensorAccType()) -> true
    | _ -> false
    end;

  local justTensor :: Boolean =
    case bounds.typerep of
    | extType(_, tensorType(_)) -> true
    | _ -> false
    end;

  local tensor :: TensorExpr =
    bounds.tensorExp;
  tensor.fmts = tm:empty();

  local fmt :: TensorFormat =
    if tensorAcc
    then getTensorFormat(tensor, tm:empty())
    else 
      case bounds.typerep of
      | extType(_, tensorType(f)) -> new(f.tensorFormat)
      | _ -> error("fmt demanded when not a tensorType")
      end;

  local access :: [Either<Expr String>] =
    if tensorAcc
    then tensor.iterAccess
    else
      map(
        \ i::Integer ->
          right(s"__v${toString(i)}")
        ,
        makeList(inc, 0, fmt.dimensions)
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
                        $stringLiteralExpr{s"Size out of bounds in foreach loop. (At ${getParsedOriginLocationOrFallback(e.fst.fromLeft).unparse})\n"});
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
                        $stringLiteralExpr{s"Size out of bounds in foreach loop. (At ${getParsedOriginLocationOrFallback(e.fst.fromLeft).unparse})\n"});
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
                      $stringLiteralExpr{s"Size out of bounds in foreach loop. (At ${getParsedOriginLocationOrFallback(e.fst.fromLeft).unparse})\n"});
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
                      $stringLiteralExpr{s"Size out of bounds in foreach loop. (At ${getParsedOriginLocationOrFallback(e.fst.fromLeft).unparse})\n"});
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
      zip(
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
          struct $name{s"tensor_${fmt.proceduralName}"} __t = (struct $name{s"tensor_${fmt.proceduralName}"}) $Expr{bounds};
        }
      end
      ::
      ableC_Stmt {
        $name{s"tensor_pack_${fmt.proceduralName}"}(_tn);
        pthread_rwlock_rdlock(&(_tn->lock));
        double* __data = _tn->data;
        unsigned long index[$intLiteralExpr{fmt.dimensions}];
      }
      ::
      map(
        \ p::Pair<Integer Pair<Integer Integer>> ->
          if p.snd.snd == storeDense
          then
            ableC_Stmt {
              unsigned long $name{s"size_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][0][0];
            }
          else
            ableC_Stmt {
              unsigned long* $name{s"pos_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][0];
              unsigned long* $name{s"idx_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][1];
              unsigned long $name{s"size_${toString(p.fst+1)}"} = _tn->dims[$intLiteralExpr{p.snd.fst}];
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
        ableC_Stmt {
          struct $name{s"tensor_${fmt.proceduralName}"}* _tn = 
              (struct $name{s"tensor_${fmt.proceduralName}"}*) &$name{nm};
        }, 
        seqStmt(
          foldr(
            \ nw::Stmt inn::Stmt ->
              seqStmt(nw, inn)
            ,
            loops,
            init
          ),
          ableC_Stmt {
            pthread_rwlock_unlock(&(_tn->lock));
          }
        )
      )
    );

  fwrd.env = top.env;
  fwrd.controlStmtContext = top.controlStmtContext;

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
  -- A break wouldn't behave as expected because we translate into multiple
  -- loops, we could translate it to work as expected (possibly a TODO)
  body.controlStmtContext = controlStmtContext(top.controlStmtContext.returnType,
      false, true, tm:empty());

  local lErrors :: [Message] =
    --errFromOrigin(var, s"Tensor Acc? ${toString(tensorAcc)}") ::
    --errFromOrigin(var, s"Tensor Access? ${toString(length(access))}") ::
    --testing(bounds) ::
    checkTensorHeader(top.env)
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
      | _ -> [errFromOrigin(bounds, s"Tensor for-each loop expected a tensor type. Instead got ${showType(bounds.typerep)}.")]
      end
    else [errFromOrigin(bounds, s"Tensor for-each loop expected a tensor access expression. Instead got ${showType(bounds.typerep)}.")];

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

function tensorVals
[Stmt] ::= ex::TensorExpr fmt::TensorFormat env::Decorated Env
{
  local nm::String = getTensorName(ex);
  local loc::Location = getParsedOriginLocation(ex).fromJust;

  return
    case ex of
    | tensorAccess(e, _, _) ->
      case decorate e with {env=env;
                      controlStmtContext=initialControlStmtContext;} of
      | declRefExpr(name(_)) -> nullStmt()
      | _ -> 
        ableC_Stmt {
          struct $name{s"tensor_${fmt.proceduralName}"} $name{s"_tensor_${toString(loc.line)}_${toString(loc.column)}"} = (struct $name{s"tensor_${fmt.proceduralName}"}) $Expr{e};
        }
      end
    | _ -> nullStmt()
    end
    ::
    ableC_Stmt {
      $name{s"tensor_pack_${fmt.proceduralName}"}(_tn);
    }
    ::
    ableC_Stmt {
      pthread_rwlock_rdlock(&(_tn->lock));
    }
    ::
    ableC_Stmt {
      double* __data = _tn->data;
    }
    ::
    flatMap(
      \ p::Pair<Integer Pair<Integer Integer>> ->
        if p.snd.snd == storeDense
        then
          ableC_Stmt {
            unsigned long $name{s"size_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][0][0];
          } :: []
        else
          ableC_Stmt {
            unsigned long* $name{s"pos_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][0];
          } ::
          ableC_Stmt { 
            unsigned long* $name{s"idx_${toString(p.fst+1)}"} = _tn->indices[$intLiteralExpr{p.snd.fst}][1];
          } ::
          ableC_Stmt {
            unsigned long $name{s"size_${toString(p.fst+1)}"} = _tn->dims[$intLiteralExpr{p.snd.fst}];
          }
          :: []
      ,
      fmt.storage
    );
}

function test
String ::= e::Decorated Expr
{
  return
    case e of
    | decExpr(a) -> s"decExpr(${test(a)})"
    | qualifiedExpr(_, a) -> s"qualifiedExpr(${test(a)})"
    | transformedExpr(_, a) -> s"transformedExpr(${test(a)})"
    | directRefExpr(id) -> s"directRefExpr(${id.name})"
    | declRefExpr(id) -> s"declRefExpr(${id.name})"
    | stringLiteral(s) -> s"stringLiteral(${s})"
    | parenExpr(a) -> s"parenExpr(${test(a)})"
    | arraySubscriptExpr(x, _) -> s"arraySubscriptExpr(${showType(x.typerep)}) : ${showType(e.typerep)}"

    | addTensor(_, _) -> "addTensor"
    | divTensor(_, _) -> "divTensor"
    | mulTensor(_, _) -> "mulTensor"
    | subTensor(_, _) -> "subTensor"
    | accessTensor(_, _) -> "accessTensor"
    | _ -> "other..."
    end;
}
