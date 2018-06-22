grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function codeGen
Stmt ::= nm::Name access::[String] expr::TensorExpr env::Decorated Env loc::Location
{
  local tensors::[Name] = nm :: getTensors(expr);
  local formats::[TensorFormatItem] = getFormats(tensors, env);

  local acc::[String] =
    findOrder(access,  parseOrder(expr, env));

  local assign :: TensorAssignExpr =
    assignExpr(nm, access, expr, tensors, formats, location=loc);

  local exprs::[Expr] = getExprs(expr);
  local exprN::[String] =
    map(
      \ e::Expr
      -> "_expr_" ++
         toString(e.location.line) ++
         "_" ++
         toString(e.location.column)
      ,
      exprs
    );

  local fwrd::Stmt =
    substStmt(
      generateExprSubs(exprs, exprN),
      parseStmt(s"""
        {
          printf("${evalExpr(expr)}\n");
          ${pack_tensors(tensors, formats)}
          ${check_dims(assign, acc)}
          // TODO: setup output matrix to have proper spaces
          ${setup_gen(tensors, formats)}
          ${setup_var(acc)}
          ${code_gen(assign, acc, loc)}
        }
      """)
    );

  return if null(acc)
         then warnStmt([err(loc, s"Cannot generate code for this tensor calculation due to cyclical access pattern")])
         else fwrd;
}

function generateExprSubs
[Substitution] ::= ex::[Expr] nm::[String]
{
  return 
    if null(ex)
    then []
    else declRefSubstitution(head(nm), head(ex)) ::
         generateExprSubs(tail(ex), tail(nm));
}

function pack_tensors
String ::= tensors::[Name] formats::[TensorFormatItem]
{
  return
    if null(tensors)
    then s""
    else s"""
      tensor_pack_${head(formats).proceduralName}(${head(tensors).name});
      ${pack_tensors(tail(tensors), tail(formats))}
    """;
}

function check_dims
String ::= expr::TensorAssignExpr acc::[String]
{
  return
    s"""
      char error = 0;
      ${implode("\n",
        map(
          check_var(expr, _),
          acc
        )
      )}
      if(error) {
        exit(1);
      }
    """;
}

function check_var
String ::= expr::TensorAssignExpr iv::String
{
  local accesses::[Pair<String Integer>] =
    findAccesses(expr, iv);
  
  return
    if null(accesses) || null(tail(accesses))
    then s""
    else 
      let h::Pair<String Integer>
        = head(accesses)
      in
      let nm::String = h.fst
      in
      let dim::String = toString(h.snd)
      in
      implode("\n",
        map(
          \ p::Pair<String Integer>
          -> s"""
            if(${nm}->dims[${dim}] != ${p.fst}->dims[${toString(p.snd)}]) {
              fprintf(stderr, "Tensors ${nm} and ${p.fst} do not have the same dimensionality for ${iv}.\n");
              error = 1;
            }
          """
          ,
          tail(accesses)
        )
      )
      end
      end
      end;      
}

function setup_gen
String ::= tensors::[Name] formats::[TensorFormatItem]
{
  return 
    if null(tensors)
    then s""
    else s"""
      ${setup_tensor(head(tensors), head(formats))}
      ${setup_gen(tail(tensors), tail(formats))}
    """;
}

function setup_var
String ::= order::[String]
{
  return
    if null(order)
    then ""
    else s"""
      unsigned long ${head(order)} = 0;
      ${setup_var(tail(order))}
    """;
}

function setup_tensor
String ::= tensor::Name format::TensorFormatItem
{
  local nm::String = tensor.name;
  
  return s"""
    unsigned long p${nm}0 = 0;
    ${implode(
        "\n",
        map(
          \p::Pair<Integer Integer> ->
            let type::Integer = case getElem(format.specifiers, p.fst) of
                                | nothing() -> 0
                                | just(i) -> i
                                end
            in let num::String = toString(p.snd + 1)
            in
            if type == storeDense
            then s"unsigned long ${nm}${num}_size = ${nm}->dims[${toString(p.fst)}];"
            else s"""
              unsigned long* ${nm}${num}_pos = ${nm}->indices[${toString(p.fst)}][0];
              unsigned long* ${nm}${num}_idx = ${nm}->indices[${toString(p.fst)}][1];
            """
            end
            end
          , 
          zipWith(pair(_, _), format.dimenOrder, defaultOrder(format.dimens))
        )
    )}
  """;
}

function code_gen
String ::= expr::TensorAssignExpr order::[String] loc::Location
{
  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);

  local iv::String = head(order);

  return 
    if null(order)
    then ""
    else 
  s"""
    ${implode("\n",
        map(
          \si::Pair<String Integer> ->
          let Dj::String = s"${si.fst}${toString(si.snd)}"
          in let Dj1::String = s"${si.fst}${toString(si.snd - 1)}"
          in
          s"unsigned long p${Dj} = ${Dj}_pos[p${Dj1}];"
          end end
          ,
          sparse_assign(expr, iv) ++ sparse_dimensions(optimized, iv)
        )
      )}
    ${implode("\n",
      map(
        \ p::LatticePoints
       -> 
          let sparse::[Pair<String Integer>] =
            sparse_dimensions(builtLattice([p]), iv)
          in let dense::[Pair<String Integer>] =
            dense_dimensions(builtLattice([p]), iv)
          in
          s"""
          while(${until_any_exhausted(merged_dimensions(p, expr.tensorFormat), expr, iv)}) {
            ${implode("\n",
              map(
                \si::Pair<String Integer> ->
                  let Dj::String = s"${si.fst}${toString(si.snd)}"
                  in
                  s"unsigned long ${iv}${si.fst} = ${Dj}_idx[p${Dj}];"
                  end
                ,
                sparse
              )
            )}
            
            ${if !null(sparse)
              then s"${iv} = ${generate_min(map(\si::Pair<String Integer> -> s"${iv}${si.fst}", sparse))};"
              else s""
            }
            
            ${implode("\n",
              map(
                \si::Pair<String Integer> ->
                  let Dj::String = s"${si.fst}${toString(si.snd)}"
                  in let Dj1::String = s"${si.fst}${toString(si.snd - 1)}"
                  in
                  s"unsigned long p${Dj} = (p${Dj1} * ${Dj}_size) + ${iv};"
                  end end
                ,
                dense
              )
            )}
            
            // section 6.2, emit-available-expressions
            
            if(0) {}
            
            ${let points::[LatticePoints] =
                p :: sub_points(p)
              in
              if listLength(points) == 0 || isAccessCond(head(points).conds) ||
                 isNullCond(head(points).conds) || isAllCond(head(points).conds)
              then 
                let pnt::LatticePoints =
                  head(points)
                in
                s"""
                  else {
                    ${code_gen(pnt.exprs, tail(order), loc)}
                    
                    // section 6.2, emit-reduction-compute
                    // section 6.3, emit-index-assembly
                    // section 6.2, emit-compute
                    printf("${exprToString(pnt.exprs)}");
                  }
                """
                end
              else 
                implode("\n",
                  map(
                    \ pnt::LatticePoints
                    -> let sd::[Pair<String Integer>] = sparse_dimensions(builtLattice([pnt]), iv)
                       in
                       s"""
                          ${if null(sd)
                            then s""
                            else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}){"
                          }
                     
                          ${code_gen(pnt.exprs, tail(order), loc)}
                          
                          // #7, emit-reduction-compute
                          // #7, emit-index-assembly
                          // #7, emit-compute()
                          printf("${exprToString(pnt.exprs)}");
                          
                          ${if null(sd)
                            then ""
                            else "}"
                          }
                       """
                       end
                    ,
                    points
                  )
                )
              end
            }
            
            ${if listLength(sparse) == 0
              then s"${iv}++;"
              else if listLength(sparse) == 1
              then let si::Pair<String Integer> =
                     head(sparse)
                   in let Dj::String = s"${si.fst}${toString(si.snd)}"
                   in
                   s"p${Dj}++;"
                   end
                   end
              else 
                implode("\n",
                map(
                  \ si::Pair<String Integer>
                  -> let Dj::String = s"${si.fst}${toString(si.snd)}"
                     in s"if(${iv}${si.fst} == ${iv}) p${Dj}++;"
                     end
                  ,
                  sparse
                )
            )}
          }
        """
        end
        end
        , 
        flatMap(
          \ p::LatticePoints
          -> p :: sub_points(p)
          ,
          optimized.points
        )
      )
    )}
  """;
}

function equals_iv
String ::= dim::Pair<String Integer> iv::String
{
  return s"${iv}${dim.fst} == ${iv}";
}

function generate_min
String ::= vars::[String]
{
  return 
    if null(tail(vars))
    then head(vars)
    else
    s"({unsigned long min = ${generate_min(tail(vars))}; ${head(vars)} < min ? ${head(vars)} : min; })";
}

function until_any_exhausted
String ::= dims::[Pair<String Integer>] expr::TensorAssignExpr var::String
{
  local dim::String =
    let dm::Pair<String Integer> =
      findDenseDimension(expr, var)
    in
    s"${dm.fst}${toString(dm.snd)}"
    end;

  return
    if null(dims)
    then s"${var} < ${dim}_size"
    else until_any_exhausted_helper(dims);
}

function until_any_exhausted_helper
String ::= dims::[Pair<String Integer>]
{
  local pair::Pair<String Integer> = head(dims);
  local nm::String = pair.fst;
  local dim::String = toString(pair.snd);

  local cond::String =
    s"p${nm}${toString(pair.snd+1)} < ${nm}${toString(pair.snd+1)}_pos[p${nm}${dim}+1]";

  return
    if null(tail(dims))
    then s"${cond}"
    else s"${cond} && ${until_any_exhausted_helper(tail(dims))}";
}

function repString
String ::= s::String i::Integer
{
  return if i <= 0
         then ""
         else s ++ repString(s, i-1);
}

function pointToString
String ::= p::LatticePoints i::Integer
{
  return s"""${repString("   ", i)}Point with Expr ${exprToString(p.exprs)} when ${condToString(p.conds)}\n${implode("\\n", map(pointToString(_, i+1), p.points))}""";
}

function exprToString
String ::= e::TensorAssignExpr
{
  return s"${show(100, e.tensorAssign.pp)} = ${show(100, e.tensorValue.pp)}";
}

function condToString
String ::= c::TensorCond
{
  return if isNullCond(c)
         then s"null"
         else if isAllCond(c)
         then s"all"
         else if isAccessCond(c)
         then case head(c.tensorElems) of
              | left(tn) -> s"${tn.name}(${toString(c.tensorDim)})"
              | _ -> s"Error Access"
              end
         else if isAndCond(c)
         then case head(c.tensorElems), head(tail(c.tensorElems)) of
              | right(l), right(r) -> s"(${condToString(l)} & ${condToString(r)})"
              | _, _ -> s"Error And"
              end
         else if isOrCond(c)
         then case head(c.tensorElems), head(tail(c.tensorElems)) of
              | right(l), right(r) -> s"(${condToString(l)} | ${condToString(r)})"
              | _, _ -> s"Error Or"
              end
         else s"Unrecognized cond";
}

function evalExpr
String ::= e::TensorExpr
{
  return
    case e of
    | nullTensorExpr() -> ""
    | access(nm, acc) -> s"${nm.name}->data[p${nm.name}${toString(listLength(acc))}]"
    | tExpr(expr) -> 
        s"_expr_${toString(expr.location.line)}_${toString(expr.location.column)}"
    | add(l, r) -> s"(${evalExpr(l)} + ${evalExpr(r)})"
    | mul(l, r) -> s"(${evalExpr(l)} * ${evalExpr(r)})"
    end;
}
