grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

imports edu:umn:cs:melt:exts:ableC:tensorAlgebra;

abstract production tensorDeepCopy
top::Expr ::= l::Expr r::Expr
{
  propagate substituted;
  top.pp = ppConcat([
      l.pp,
      text("="),
      r.pp
    ]);

  local formatL :: TensorFormat =
    case l.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    end;

  local formatR :: TensorFormat =
    case r.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    end;
  
  local lErrors :: [Message] =
    case l.typerep, r.typerep of
    | tensorType(_, _, _), tensorType(_, _, _) -> 
      if formatL.dimensions == formatR.dimensions
      then []
      else [err(top.location, "Format changes can only be performed between tensors of the same order.")]
    | tensorType(_, _, _), _ -> 
      case r of
      | build_empty(_, _) -> []
      | build_data(_, _) -> []
      | buildTensorExpr(_, _) -> []
      | _ -> [err(top.location, "Tensor Deep Copy can only be performed on tensors. (This error should not occur)")]
      end
    | _, _ -> [err(top.location, "Tensor Deep Copy can only be performed on tensors. (This error should not occur)")]
    end
    ++
    l.errors
    ++
    r.errors;

  local fwrd :: Expr =
    case r of
    | build_empty(_, _) ->
      eqExpr(l, r, location=top.location)
    | build_data(_, _) -> 
      eqExpr(l, r, location=top.location)
    | buildTensorExpr(_, _) -> 
      eqExpr(l, r, location=top.location)
    | declRefExpr(_) ->
      if formatL.proceduralName == formatR.proceduralName
      then
        ableC_Expr {
          ({
            if($Expr{l}.dims) free($Expr{l}.dims);
            if($Expr{l}.indices) { $Stmt{freeIndices(l, formatL)}; free($Expr{l}.indices); }
            if($Expr{l}.data) free($Expr{l}.data);
            if($Expr{l}.buffer) __free_tensor_tree($Expr{l}.buffer);
            $Expr{l}.bufferCnt = 0;
            $Expr{l}.buffer = calloc(1, sizeof(struct __tensor_tree));
            $Expr{l}.buffer->children = calloc(1, sizeof(struct __tensor_tree));
            $Expr{l}.form = "";
            
            $name{s"tensor_pack_${formatR.proceduralName}"}(&$Expr{r});
            
            $Expr{l}.dims = calloc($intLiteralExpr{formatL.dimensions}, sizeof(unsigned long));
            memcpy($Expr{l}.dims, $Expr{r}.dims, sizeof(unsigned long) * $intLiteralExpr{formatL.dimensions});

            unsigned long size = 1;
            $Expr{l}.indices = calloc($intLiteralExpr{formatL.dimensions}, sizeof(unsigned long**));
            $Stmt{copyIndices(l, r, formatL)}

            $Expr{l}.data = calloc($Expr{r}.dataLen, sizeof(double));
            memcpy($Expr{l}.data, $Expr{r}.data, sizeof(double) * $Expr{r}.dataLen);
            $Expr{l}.dataLen = $Expr{r}.dataLen;

            pthread_rwlock_destroy(&($Expr{l}.lock));
            pthread_rwlock_init(&($Expr{l}.lock), 0);

            $Expr{l};
          })
        }
      else
        ableC_Expr {
        ({
          if($Expr{l}.indices) { $Expr{freeTensor(l, location=top.location)}; }
          // uses .indices as a check of whether things are initialized

          memset(&$Expr{l}, 0, sizeof(struct $name{s"tensor_${formatL.proceduralName}"}));
          $name{s"tensor_make_${formatL.proceduralName}"}(&$Expr{l}, $Expr{r}.dims);

          unsigned long __idx[$intLiteralExpr{formatL.dimensions}];

          double* data = $Expr{r}.data;
          $Stmt {
            foldl(
              \ abv::Stmt p::Pair<Integer Pair<Integer Integer>> ->
                if p.snd.snd == storeDense
                then
                  ableC_Stmt {
                    $Stmt{abv}
                    unsigned long $name{s"size_${toString(p.fst+1)}"} = 
                      $Expr{r}.indices[$intLiteralExpr{p.snd.fst}][0][0];
                  }
                else
                  ableC_Stmt {
                    $Stmt{abv}
                    unsigned long* $name{s"pos_${toString(p.fst+1)}"} =
                      $Expr{r}.indices[$intLiteralExpr{p.snd.fst}][0];
                    unsigned long* $name{s"idx_${toString(p.fst+1)}"} =
                      $Expr{r}.indices[$intLiteralExpr{p.snd.fst}][1];
                  }
              ,
              nullStmt(),
              formatR.storage
            )
          }

          $Stmt {
            foldr(
              \ d::Pair<Integer Pair<Integer Integer>> inn::Stmt ->
                if d.snd.snd == storeDense
                then
                  ableC_Stmt {
                    for(unsigned long $name{s"v${toString(d.fst+1)}"} = 0;
                        $name{s"v${toString(d.fst+1)}"} < $name{s"size_${toString(d.fst+1)}"};
                        $name{s"v${toString(d.fst+1)}"}++) {
                      __idx[$intLiteralExpr{d.fst}] = $name{s"v${toString(d.fst+1)}"};
                      $Stmt{
                        if d.fst == 0
                        then 
                          ableC_Stmt { 
                            unsigned long $name{s"p${toString(d.fst+1)}"} = 
                              $name{s"v${toString(d.fst+1)}"}; 
                          }
                        else
                          ableC_Stmt { 
                            unsigned long $name{s"p${toString(d.fst+1)}"} = 
                              ($name{s"p${toString(d.fst)}"} * $name{s"size_${toString(d.fst+1)}"}) 
                                + $name{s"v${toString(d.fst+1)}"}; 
                          }
                      }
                      $Stmt{inn}
                    }
                  }
                else
                  if d.fst == 0
                  then
                    ableC_Stmt {
                      for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                        __idx[0] = idx_1[p1];
                        $Stmt{inn}
                      }
                    }
                  else
                    ableC_Stmt {
                      for(unsigned long $name{s"p${toString(d.fst+1)}"} = 
                            $name{s"pos_${toString(d.fst+1)}"}[$name{s"p${toString(d.fst)}"}];
                          $name{s"p${toString(d.fst+1)}"} < $name{s"pos_${toString(d.fst+1)}"}
                            [$name{s"p${toString(d.fst)}"}+1];
                          $name{s"p${toString(d.fst+1)}"}++) {
                        __idx[$intLiteralExpr{d.fst}] = $name{s"idx_${toString(d.fst+1)}"}
                          [$name{s"p${toString(d.fst+1)}"}];
                        $Stmt{inn}
                      }
                    }
              ,
              ableC_Stmt {
                double v = data[$name{s"p${toString(formatL.dimensions)}"}];
                if(v != 0) { // TODO: This is unsafe
                  __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};
                  *$name{s"tensor_getPointer_${formatL.proceduralName}"}(&$Expr{l}, __idx) = v;
                }
              },
              formatR.storage
            )
          }

          $Expr{l};
        })
        }
    | _ -> eqExpr(l, r, location=top.location)
    end;

  forwards to
    mkErrorCheck(lErrors, fwrd);
}

function freeIndices
Stmt ::= expr::Expr fmt::TensorFormat
{
  return freeIndices_helper(expr, fmt.storage);
}

function freeIndices_helper
Stmt ::= expr::Expr strg::[Pair<Integer Pair<Integer Integer>>] 
{
  local p::Pair<Integer Pair<Integer Integer>> =
    head(strg);

  return
    if null(strg)
    then nullStmt()
    else if p.snd.snd == storeDense
    then
      ableC_Stmt {
        free($Expr{expr}.indices[$intLiteralExpr{p.snd.fst}][0]);
        free($Expr{expr}.indices[$intLiteralExpr{p.snd.fst}]);
        $Stmt{freeIndices_helper(expr, tail(strg))}
      }
    else
      ableC_Stmt {
        free($Expr{expr}.indices[$intLiteralExpr{p.snd.fst}][0]);
        free($Expr{expr}.indices[$intLiteralExpr{p.snd.fst}][1]);
        free($Expr{expr}.indices[$intLiteralExpr{p.snd.fst}]);
        $Stmt{freeIndices_helper(expr, tail(strg))}
      };
}

function copyIndices
Stmt ::= dest::Expr src::Expr fmt::TensorFormat
{
  return copyIndices_helper(dest, src, fmt.storage);
}

function copyIndices_helper
Stmt ::= dest::Expr src::Expr strg::[Pair<Integer Pair<Integer Integer>>]
{
  local p::Pair<Integer Pair<Integer Integer>> =
    head(strg);

  return
    if null(strg)
    then nullStmt()
    else if p.snd.snd == storeDense
    then
      ableC_Stmt {
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}] = calloc(1, sizeof(unsigned long*));
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0] = calloc(1, sizeof(unsigned long));
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0][0] = $Expr{dest}.dims[$intLiteralExpr{p.snd.fst}];
        size *= $Expr{dest}.dims[$intLiteralExpr{p.snd.fst}];
        $Stmt{copyIndices_helper(dest, src, tail(strg))}
      }
    else
      ableC_Stmt {
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}] = calloc(2, sizeof(unsigned long*));
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0] = calloc(size + 1, sizeof(unsigned long));
        memcpy($Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0], $Expr{src}.indices[$intLiteralExpr{p.snd.fst}][0], sizeof(unsigned long) * (size + 1));
        size = $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0][size];
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][1] = calloc(size, sizeof(unsigned long));
        memcpy($Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][1], $Expr{src}.indices[$intLiteralExpr{p.snd.fst}][1], sizeof(unsigned long) * (size));
        $Stmt{copyIndices_helper(dest, src, tail(strg))}
      };
}
