grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

imports edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Performs a deep copy of a tensor. Since we dynamically
   allocate and free arrays (and the buffer), it is not safe to
   make shallow copies of tensors, because the array that one
   points to may be free'd by a function call to the other.
   This production can also perform format changes for tensors.
   This is not a thread-safe operation.
-}
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
    | extType(_, tensorType(f)) -> new(f.tensorFormat)
    end;

  local formatR :: TensorFormat =
    case r.typerep of
    | extType(_, tensorType(f)) -> new(f.tensorFormat)
    end;
  
  local lErrors :: [Message] =
    case l.typerep, r.typerep of
    | extType(_, tensorType(_)), extType(_, tensorType(_)) ->
      if formatL.dimensions == formatR.dimensions
      then []
      else [err(top.location, "Format changes can only be performed between tensors of the same order.")]
    | extType(_, tensorType(_)), _ ->
      case r of
      | build_empty(_, _) -> []
      | build_data(_, _) -> []
      | buildTensorExpr(_, _) -> []
      | directCallExpr(_, _) -> []
      | callExpr(_, _) -> []
      | _ -> [err(top.location, "Tensor Deep Copy can only be performed on tensors. (This error should not occur)")]
      end
    | _, _ -> [err(top.location, "Tensor Deep Copy can only be performed on tensors. (This error should not occur)")]
    end
    ++
    l.errors
    ++
    r.errors;

  {- We only want to perform a deep copy if the right-hand side is a tensor,
     not a function (or a build) that creates a tensor. Unfortunatly, there's
     no perfect way to detect this, we we only deep copy for declRefExpr's 
  -}
  local fwrd :: Expr =
    case r of
    | declRefExpr(_) ->
      if formatL.proceduralName == formatR.proceduralName
      then
        ableC_Expr {
          ({
            struct $name{s"tensor_${formatL.proceduralName}"}* _l = (struct $name{s"tensor_${formatL.proceduralName}"}*) &$Expr{l};
            struct $name{s"tensor_${formatR.proceduralName}"}* _r = (struct $name{s"tensor_${formatR.proceduralName}"}*) &$Expr{r};

            if(_l->dims) free(_l->dims);
            if(_l->indices) { $Stmt{freeIndices(ableC_Expr{*_l}, formatL)}; free(_l->indices); }
            if(_l->data) free(_l->data);
            if(_l->buffer) __free_tensor_tree(_l->buffer);
            _l->bufferCnt = 0;
            _l->buffer = calloc(1, sizeof(struct __tensor_tree));
            _l->buffer->children = calloc(1, sizeof(struct __tensor_tree));
            _l->form = "";
            
            $name{s"tensor_pack_${formatR.proceduralName}"}(_r);
            
            _l->dims = calloc($intLiteralExpr{formatL.dimensions}, sizeof(unsigned long));
            memcpy(_l->dims, _r->dims, sizeof(unsigned long) * $intLiteralExpr{formatL.dimensions});

            unsigned long size = 1;
            _l->indices = calloc($intLiteralExpr{formatL.dimensions}, sizeof(unsigned long**));
            $Stmt{copyIndices(ableC_Expr{*_l}, ableC_Expr{*_r}, formatL)}

            _l->data = calloc(_r->dataLen, sizeof(double));
            memcpy(_l->data, _r->data, sizeof(double) * _r->dataLen);
            _l->dataLen = _r->dataLen;

            pthread_rwlock_destroy(&(_l->lock));
            pthread_rwlock_init(&(_l->lock), 0);

            *_l;
          })
        }
      else
        ableC_Expr {
        ({
          struct $name{s"tensor_${formatL.proceduralName}"}* _l = (struct $name{s"tensor_${formatL.proceduralName}"}*) &$Expr{l};
          struct $name{s"tensor_${formatR.proceduralName}"}* _r = (struct $name{s"tensor_${formatR.proceduralName}"}*) &$Expr{r};
         
          if(_l->indices) { $Expr{freeTensor(l, location=top.location)}; }
          // uses .indices as a check of whether things are initialized

          __tensor_location = $stringLiteralExpr{let loc::Location = top.location in s"At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)}" end};

          memset(_l, 0, sizeof(struct $name{s"tensor_${formatL.proceduralName}"}));
          $name{s"tensor_make_${formatL.proceduralName}"}(_l, _r->dims);

          unsigned long __idx[$intLiteralExpr{formatL.dimensions}];

          double* __data = _r->data;
          $Stmt {
            foldl(
              \ abv::Stmt p::Pair<Integer Pair<Integer Integer>> ->
                if p.snd.snd == storeDense
                then
                  ableC_Stmt {
                    $Stmt{abv}
                    unsigned long $name{s"size_${toString(p.fst+1)}"} = 
                      _r->indices[$intLiteralExpr{p.snd.fst}][0][0];
                  }
                else
                  ableC_Stmt {
                    $Stmt{abv}
                    unsigned long* $name{s"pos_${toString(p.fst+1)}"} =
                      _r->indices[$intLiteralExpr{p.snd.fst}][0];
                    unsigned long* $name{s"idx_${toString(p.fst+1)}"} =
                      _r->indices[$intLiteralExpr{p.snd.fst}][1];
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
                double v = __data[$name{s"p${toString(formatL.dimensions)}"}];
                if(v != 0) { // TODO: This is unsafe
                  *$name{s"tensor_getPointer_${formatL.proceduralName}"}(_l, __idx) = v;
                }
              },
              formatR.storage
            )
          }

          *_l;
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
