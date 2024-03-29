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
      \ v::String -> !contains(v, rAcc),
      lAcc
    )
    ++
    filter(
      \ v::String -> !contains(v, lAcc),
      rAcc
    );

  local mapping :: [Integer] =
    map(
      \ v::String ->
        positionOf(v, lAcc)
      ,
      rAcc
    );

  -- Pairs used to access the right hand side tensor in proper order and
  -- then correspond the index to the correct index in on the left-hand side
  local access:: [((Integer, Integer), (Integer, Integer))] =
    zipWith(
      \ r::(Integer, Integer, Integer) lNum :: Integer ->
        (
          (r.1, getElem(lFmt.storage, lNum).fromJust.2),
          r.snd
        )
      ,
      rFmt.storage,
      mapping
    );

  local lFmtNm :: String = lFmt.proceduralName;
  local rFmtNm :: String = rFmt.proceduralName;

  forwards to
    if !null(missing)
    then 
      errorExpr(
        [errFromOrigin(top, s"Tensor tranpose failed because the variable(s) ${implode(", ", missing)} appear only on one side.")]
      )
    else
      ableC_Expr {
      ({
        char error = 0;
        $Stmt{
          foldl(
            \ inn::Stmt var::String ->
              let idx::Integer =
                positionOf(var, rAcc)
              in let e::Pair<Pair<Integer Integer> Pair<Integer Integer>> =
                getElem(access, idx).fromJust
              in let lPos::Integer = e.fst.snd in
              let rPos::Integer = e.snd.fst in
              ableC_Stmt {
                if(((struct $name{s"tensor_${lFmtNm}"})$name{lNm}).dims[$intLiteralExpr{lPos}] 
                  != ((struct $name{s"tensor_${rFmtNm}"})$name{rNm}).dims[$intLiteralExpr{rPos}]) {
                  fprintf(stderr, 
                    $stringLiteralExpr{s"Tensors ${lNm} and ${rNm} do not have the same dimensionality for ${var} (At ${getParsedOriginLocationOrFallback(top).unparse})"});
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

        memset(((struct $name{s"tensor_${lFmtNm}"})$name{lNm}).data, 0, 
          ((struct $name{s"tensor_${rFmtNm}"})$name{lNm}).dataLen * sizeof(double));
        __free_tensor_tree(((struct $name{s"tensor_${lFmtNm}"})$name{lNm}).buffer);
        
        ((struct $name{s"tensor_${lFmtNm}"}*) &$name{lNm})->buffer = calloc(1, sizeof(struct __tensor_tree));
        ((struct $name{s"tensor_${lFmtNm}"}*) &$name{lNm})->buffer->children = calloc(1, sizeof(struct __tensor_tree));

        $name{s"tensor_pack_${rFmt.proceduralName}"}((struct $name{s"tensor_${rFmtNm}"}*) &$name{rNm});
        pthread_rwlock_rdlock(&(((struct $name{s"tensor_${rFmtNm}"}*) &$name{rNm})->lock));

        double* __data = ((struct $name{s"tensor_${rFmtNm}"}) $name{rNm}).data;
        $Stmt {
          foldl(
            \ abv::Stmt p::Pair<Integer Pair<Integer Integer>> ->
              if p.snd.snd == storeDense
              then
                ableC_Stmt {
                  $Stmt{abv}
                  unsigned long $name{s"size_${toString(p.fst+1)}"} =
                    ((struct $name{s"tensor_${rFmtNm}"}) $name{rNm}).indices[$intLiteralExpr{p.snd.fst}][0][0];
                }
              else
                ableC_Stmt {
                  $Stmt{abv}
                  unsigned long* $name{s"pos_${toString(p.fst+1)}"} =
                    ((struct $name{s"tensor_${rFmtNm}"}) $name{rNm}).indices[$intLiteralExpr{p.snd.fst}][0];
                  unsigned long* $name{s"idx_${toString(p.fst+1)}"} =
                    ((struct $name{s"tensor_${rFmtNm}"}) $name{rNm}).indices[$intLiteralExpr{p.snd.fst}][1];
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
              double v = __data[$name{s"p${toString(lFmt.dimensions)}"}];
              if(v != 0.0) {
                *$name{s"tensor_getPointer_locked_${lFmt.proceduralName}"}(
                  (struct $name{s"tensor_${lFmtNm}"}*) &$name{lNm}, __idx) = v;
              }
            },
            access
          )
        }

        $name{lNm};
      })
      };
}
