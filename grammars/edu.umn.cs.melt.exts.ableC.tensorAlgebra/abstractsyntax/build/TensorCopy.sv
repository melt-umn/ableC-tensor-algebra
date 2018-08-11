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
    case l.typerep of
    | tensorType(_, f, _) -> new(f.tensorFormat)
    end;

  local lErrors :: [Message] =
    case l.typerep, r.typerep of
    | tensorType(_, _, _), tensorType(_, _, _) -> []
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
    if formatL.proceduralName == formatR.proceduralName
    then
      case r of
      | build_empty(_, _) ->
        eqExpr(l, r, location=top.location)
      | build_data(_, _) -> 
        eqExpr(l, r, location=top.location)
      | buildTensorExpr(_, _) -> 
        eqExpr(l, r, location=top.location)
      | _ ->
        ableC_Expr {
          ({
            if($Expr{l}.dims) free($Expr{l}.dims);
            if($Expr{l}.indices) { $Stmt{freeIndices(l, formatL)}; free($Expr{l}.indices); }
            if($Expr{l}.data) free($Expr{l}.data);
            $Expr{l}.bufferCnt = 0;
            $Expr{l}.buffer.isLeaf = 0;
            $Expr{l}.buffer.index = 0;
            $Expr{l}.buffer.numChildren = 0;
            if($Expr{l}.buffer.children) free($Expr{l}.buffer.children);
            $Expr{l}.buffer.children = 0;
            $Expr{l}.form = "";
            
            $name{s"tensor_pack_${formatR.proceduralName}"}(&$Expr{r});
            
            $Expr{l}.dims = malloc(sizeof(unsigned long) * $intLiteralExpr{formatL.dimensions});
            memcpy($Expr{l}.dims, $Expr{r}.dims, sizeof(unsigned long) * $intLiteralExpr{formatL.dimensions});

            unsigned long size = 1;
            $Expr{l}.indices = malloc(sizeof(unsigned long**) * $intLiteralExpr{formatL.dimensions});
            $Stmt{copyIndices(l, r, formatL)}

            $Expr{l}.data = malloc(sizeof(double) * $Expr{r}.dataLen);
            memcpy($Expr{l}.data, $Expr{r}.data, sizeof(double) * $Expr{r}.dataLen);
            $Expr{l}.dataLen = $Expr{r}.dataLen;

            $Expr{l};
          })
        }
      end
    else
      ableC_Expr {
        ({
          fprintf(stderr, "Tensor format changes are currently not supported through assignment.");
          exit(0);
          0;
        })
      };

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
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}] = malloc(sizeof(unsigned long*));
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0] = &($Expr{dest}.dims[$intLiteralExpr{p.snd.fst}]);
        size *= $Expr{dest}.dims[$intLiteralExpr{p.snd.fst}];
        $Stmt{copyIndices_helper(dest, src, tail(strg))}
      }
    else
      ableC_Stmt {
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}] = malloc(sizeof(unsigned long*) * 2);
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0] = malloc(sizeof(unsigned long) * (size + 1));
        memcpy($Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0], $Expr{src}.indices[$intLiteralExpr{p.snd.fst}][0], sizeof(unsigned long) * (size + 1));
        size = $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][0][size];
        $Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][1] = malloc(sizeof(unsigned long) * size);
        memcpy($Expr{dest}.indices[$intLiteralExpr{p.snd.fst}][1], $Expr{src}.indices[$intLiteralExpr{p.snd.fst}][1], sizeof(unsigned long) * (size));
        $Stmt{copyIndices_helper(dest, src, tail(strg))}
      };
}
