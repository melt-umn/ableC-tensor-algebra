grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:build;

{- Production for transposing a tensor. This is called by TensorExpreAssign,
   the main code-generation production, in cases where both the left and 
   right sides are simply tensor accesses, and an order for index variable
   access cannot be produced. Transpose only works when both tensors are
   of the same order.
-}
abstract production transpose
top::Expr ::= lNm::String lAcc::[String] lFmt::TensorFormat
              rNm::String rAcc::[String] rFmt::TensorFormat
{
  -- Test if either side has variables not on the other
  local missing :: [String] =
    filter(
      \ v::String -> !containsBy(stringEq, v, rAcc),
      lAcc
    )
    ++
    filter(
      \ v::String -> !containsBy(stringEq, v, lAcc),
      rAcc
    );

  local mapping :: [Integer] =
    map(
      \ v::String ->
        positionOf(stringEq, v, lAcc)
      ,
      rAcc
    );

  -- Pairs used to access the right hand side tensor in proper order and
  -- then correspond the index to the correct index in on the left-hand side
  local access:: [Pair< Pair<Integer Integer> Pair<Integer Integer> >] =
    zipWith(
      \ r::Pair<Integer Pair<Integer Integer>> lNum :: Integer ->
        pair(
          pair(r.fst, let e::Pair<Integer Pair<Integer Integer>> = getElem(lFmt.storage, lNum).fromJust in e.snd.fst end),
          r.snd
        )
      ,
      rFmt.storage,
      mapping
    );

  forwards to
    if !null(missing)
    then 
      errorExpr(
        [err(top.location, s"Tensor tranpose failed because the variable(s) ${implode(", ", missing)} appear only on one side.")],
        location=top.location
      )
    else
      ableC_Expr {
      ({
        char error = 0;
        $Stmt{
          foldl(
            \ inn::Stmt var::String ->
              let idx::Integer =
                positionOf(stringEq, var, rAcc)
              in let e::Pair<Pair<Integer Integer> Pair<Integer Integer>> =
                getElem(access, idx).fromJust
              in let lPos::Integer = e.fst.snd in
              let rPos::Integer = e.snd.fst in
              ableC_Stmt {
                if($name{lNm}.dims[$intLiteralExpr{lPos}] != $name{rNm}.dims[$intLiteralExpr{rPos}]) {
                  fprintf(stderr, 
                    $stringLiteralExpr{let loc::Location = top.location in  s"Tensors ${lNm} and ${rNm} do not have the same dimensionality for ${var} (At ${loc.filename}, Line ${toString(loc.line)}, Col ${toString(loc.column)})" end});
                  error = 1;
                }
                $Stmt{inn}
              }
              end end end end
            ,
            ableC_Stmt {
              if(error) {
                exit(1);
              }
            },
            rAcc
          )
        }

        memset($name{lNm}.data, 0, $name{lNm}.dataLen * sizeof(double));
        __free_tensor_tree($name{lNm}.buffer);
        $name{lNm}.buffer = calloc(1, sizeof(struct __tensor_tree));
        $name{lNm}.buffer->children = calloc(1, sizeof(struct __tensor_tree));

        $name{s"tensor_pack_${rFmt.proceduralName}"}(&$name{rNm});
        pthread_rwlock_rdlock(&($name{rNm}.lock));

        double* data = $name{rNm}.data;
        $Stmt {
          foldl(
            \ abv::Stmt p::Pair<Integer Pair<Integer Integer>> ->
              if p.snd.snd == storeDense
              then
                ableC_Stmt {
                  $Stmt{abv}
                  unsigned long $name{s"size_${toString(p.fst+1)}"} =
                    $name{rNm}.indices[$intLiteralExpr{p.snd.fst}][0][0];
                }
              else
                ableC_Stmt {
                  $Stmt{abv}
                  unsigned long* $name{s"pos_${toString(p.fst+1)}"} =
                    $name{rNm}.indices[$intLiteralExpr{p.snd.fst}][0];
                  unsigned long* $name{s"idx_${toString(p.fst+1)}"} =
                    $name{rNm}.indices[$intLiteralExpr{p.snd.fst}][1];
                }
            ,
            nullStmt(),
            rFmt.storage
          )
        }

        unsigned long __idx[$intLiteralExpr{lFmt.dimensions}];
        $Stmt {
          foldr(
            \ d::Pair<Pair<Integer Integer> Pair<Integer Integer>> inn::Stmt ->
              if d.snd.snd == storeDense
              then
                ableC_Stmt {
                  for(unsigned long $name{s"v${toString(d.fst.fst+1)}"} = 0;
                      $name{s"v${toString(d.fst.fst+1)}"} < $name{s"size_${toString(d.fst.fst+1)}"};
                      $name{s"v${toString(d.fst.fst+1)}"}++) {
                    __idx[$intLiteralExpr{d.fst.snd}] = $name{s"v${toString(d.fst.fst+1)}"};
                    $Stmt{
                      if d.fst.fst == 0
                      then
                        ableC_Stmt {
                          unsigned long $name{s"p${toString(d.fst.fst+1)}"} =
                            $name{s"v${toString(d.fst.fst+1)}"};
                        }
                      else
                        ableC_Stmt {
                          unsigned long $name{s"p${toString(d.fst.fst+1)}"} = 
                            ($name{s"p${toString(d.fst.fst)}"} * $name{s"size_${toString(d.fst.fst+1)}"})
                              + $name{s"v${toString(d.fst.fst+1)}"};
                        }
                    }
                    $Stmt{inn}
                  }
                }
              else
                if d.fst.fst == 0
                then
                  ableC_Stmt {
                    for(unsigned long p1 = pos_1[0]; p1 < pos_1[1]; p1++) {
                      __idx[$intLiteralExpr{d.fst.snd}] = idx_1[p1];
                      $Stmt{inn}
                    }
                  }
                else
                  ableC_Stmt {
                    for(unsigned long $name{s"p${toString(d.fst.fst+1)}"} =
                          $name{s"pos_${toString(d.fst.fst+1)}"}[$name{s"p${toString(d.fst.fst)}"}];
                        $name{s"p${toString(d.fst.fst+1)}"} < 
                          $name{s"pos_${toString(d.fst.fst+1)}"}[$name{s"p${toString(d.fst.fst)}"}+1];
                        $name{s"p${toString(d.fst.fst+1)}"}++) {
                      __idx[$intLiteralExpr{d.fst.snd}] = 
                        $name{s"idx_${toString(d.fst.fst+1)}"}[$name{s"p${toString(d.fst.fst+1)}"}];
                      $Stmt{inn}
                    }
                  }
            ,
            ableC_Stmt {
              double v = data[$name{s"p${toString(lFmt.dimensions)}"}];
              if(v != 0.0) {
                *$name{s"tensor_getPointer_locked_${lFmt.proceduralName}"}(&$name{lNm}, __idx) = v;
              }
            },
            access
          )
        }

        $name{lNm};
      })
      };
}
