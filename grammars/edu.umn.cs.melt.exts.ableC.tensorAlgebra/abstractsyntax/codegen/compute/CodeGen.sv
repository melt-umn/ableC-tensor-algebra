grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function codeGen
Stmt ::= nm::Name access::[String] expr::TensorExpr env::Decorated Env loc::Location
{
  local tensors::[Name] = 
    nm :: 
    nubBy(
      \ n1::Name n2::Name
      -> n1.name == n2.name
      ,
      getTensors(expr)
    );
  local formats::[TensorFormatItem] = getFormats(tensors, env);

  local valueAccess::[String] =
    nubBy(
      stringEq(_, _)
      ,
      flatMap(
        \ s::[String]
        -> s
        ,
        parseOrder(expr, env)
      )
    );
  
  local invalidAccess::Boolean =
    foldl(
      \ b::Boolean
        s::String
      -> !containsBy(
           stringEq(_, _),
           s,
           valueAccess
         ) || b
      ,
      false,
      access
    );
  
  local errorVariable::[String] =
    filter(
      \ s::String 
      -> !containsBy(
           stringEq(_, _),
           s,
           valueAccess
         )
      ,
      access
    );

  local acc::[String] =
    findOrder(
      orderList(access, head(formats).dimenOrder),  
      parseOrder(expr, env)
    );

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

  expr.parenExpr = [];
  
  local exprSub::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
    exprSubs(assign, acc);
  
  local fwrd::Stmt =
    substStmt(
      generateExprSubs(exprs, exprN),
      parseStmt(s"""
        {
          ${check_dims(assign, acc)}
          ${pack_tensors(tail(tensors), tail(formats))}
          ${setup_gen(tensors, formats)}
          ${build_output(exprSub, acc, tensors, loc)}
          {
            ${setup_gen(head(tensors) :: [], head(formats) :: [])}
            ${code_gen(exprSub, acc, loc, env)}
          }
          ${implode("\n",
              map(
                \ n::Name
                -> s"""${n.name}->form = "${expr.proceduralName}";"""
                ,
                tensors
              )
          )}
        }
      """)
    );

  local localErrors::[Message] =
    if null(acc)
    then [err(loc, s"Cannot generate code for this tensor calculation due to cyclical access pattern.")]
    else []
    ++
    if invalidAccess
    then [err(loc, s"Cannot generate code for this calculation since the index variable(s) ${implode(", ", errorVariable)} only occurs on the left-hand side.")]
    else [];

  return if !null(localErrors)
         then warnStmt(localErrors)
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

function build_output
String ::= exprs::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] order::[String] tensors::[Name] loc::Location
{
  local expr::TensorAssignExpr =
    head(exprs).fst.fst;
  
  local nm::Name =
    case expr.tensorAssign of
    | access(n, _) -> n
    | _ -> name("error", location=loc)
    end;
  local out::String = nm.name;
  local fmt::TensorFormatItem =
    head(tm:lookup(nm, expr.tensorFormat));

  local acc::[String] =
    case expr.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;

  local ex::TensorExpr = expr.tensorValue;
  ex.parenExpr = [];

  return s"""
  {
    if(${implode("||", map(\n::Name -> s"""strcmp(${n.name}->form, "${ex.proceduralName}") != 0""", tensors))} ) {    
      ${out}->bufferCnt = 0;
      ${out}->buffer.numChildren = 0;
      ${out}->buffer.children = 0;
      
      unsigned long* index = GC_malloc(sizeof(unsigned long) * ${toString(fmt.dimens)});
      ${build_body(exprs, order, nm, fmt, acc, loc)}
      {
        ${build_pack(out, fmt)}
      }
    } else {
      memset(${out}->data, 0, sizeof(double) * ${out}->dataLen);
    }
  }
  """;
}

function build_pack
String ::= name::String fmt::TensorFormatItem
{
  local dims::String =
    toString(fmt.dimens);

  return s"""
    struct tensor_${fmt.proceduralName}* t = ${name};
    struct tensor_tree_s* buffer = &(t->buffer);
    unsigned long* dims = t->dims;
    
    tensor_packTree_${fmt.proceduralName}(buffer, dims);
    
    t->indices = GC_malloc(sizeof(unsigned long**) * ${dims});
    unsigned long numChildren = 1;
    struct tensor_tree_s** trees = &(buffer);
    
    struct tensor_tree_s** temp_tree;
    unsigned long total, dimSize, index, newChildren;
    
    ${generatePackBody_Assemble(fmt.dimenOrder, fmt.specifiers)}
    
    t->data = GC_malloc(sizeof(double) * numChildren);
    for(unsigned long i = 0; i < numChildren; i++) {
      t->data[i] = trees[i]->val;
    }
    t->dataLen = numChildren;
    t->bufferCnt = 0;
    t->buffer.numChildren = 0;
    t->buffer.children = 0;
  """;
}

function build_body
String ::= exprs::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] order::[String] nm::Name fmt::TensorFormatItem acc::[String] loc::Location
{
  local expr::TensorAssignExpr =
    head(exprs).fst.fst;
  
  local out::String = nm.name;

  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);
  
  local more::Boolean =
    containsAny(
      stringEq(_, _),
      order,
      acc
    );
  
  local iv::String = head(order);
  local l::Integer =
    positionOf(
      stringEq(_, _),
      iv,
      acc
    );
  local lc::String = toString(l);
  local oC::String =
    toString(
      positionOf(
        \ i1::Integer
          i2::Integer
        -> i1 == i2
        ,
        l,
        fmt.dimenOrder
      )
    );

  local out_is_sparse::Boolean =
    case sparse_assign(expr, iv) of
    | si::[] -> true
    | _ -> false
    end;
  local out_acc::Integer =
    case sparse_assign(expr, iv) of
    | si::[] -> si.snd
    | _ -> -1
    end;
  
  return
    if null(order) || !more
    then s"""
      tensor_insertBuff_${fmt.proceduralName}(&(${out}->buffer), index, 0.0);
      ${out}->bufferCnt += 1;
    """
    else s"""
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
      unsigned long ${iv} = 0;
      ${implode("\n",
          map(
            \ p::LatticePoints
            -> let sparse::[Pair<String Integer>] =
                 sparse_dimensions(builtLattice([p]), iv)
               in let dense::[Pair<String Integer>] =
                 dense_dimensions(builtLattice([p]), iv)
               in let allBelow::Boolean =
                 containsFunc(
                   \ lp::LatticePoints
                   -> isAllCond(lp.conds)
                   ,
                   p.points
                 )
               in
               s"""
                 while(${until_any_exhausted(merged_dimensions(p, expr.tensorFormat), expr, iv)}${if isAllCond(p.conds) && !null(sparse) then s" && ${implode("&&", map(\p::Pair<String Integer> -> s"p${p.fst}${toString(p.snd)} < ${p.fst}${toString(p.snd)}_pos[p${p.fst}${toString(p.snd-1)}+1]", sparse))}" else ""}) {
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
                   
                   ${if !null(sparse) && !isAllCond(p.conds)
                     then s"${iv} = ${generate_min(map(\si::Pair<String Integer> -> s"${iv}${si.fst}", sparse))};"
                     else s""
                   }
                   ${if l != -1
                     then s"""
                            index[${lc}] = ${iv};
                            tensor_insertBuff_mid_${fmt.proceduralName}(&(${out}->buffer), index, ${oC});
                          """
                     else ""
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
                   
                   
                   if(0) {}
                   
                   ${let points::[LatticePoints] = 
                       (if isAllCond(p.conds) && !null(sub_points(p)) then [] else [p]) ++ sub_points(p)
                     in
                     if listLength(points) == 0 || isNullCond(head(points).conds)
                     then
                       let pnt::LatticePoints =
                         head(points)
                       in
                       let ex::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
                         exprSubs(pnt.exprs, order)
                       in
                       s"""
                         else {
                           ${build_body(tail(ex), tail(order), nm, fmt, acc, loc)}
                         }
                       """
                       end
                       end
                     else
                       implode("\n",
                         map(
                           \ pnt::LatticePoints
                           -> let sd::[Pair<String Integer>] = sparse_dimensions(builtLattice([pnt]), iv)
                              in
                              let ex::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
                                exprSubs(pnt.exprs, order)
                              in
                              s"""
                                ${if null(sd)
                                  then s"else {"
                                  else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}) {"
                                }
                                
                                ${build_body(tail(ex), tail(order), nm, fmt, acc, loc)}
                                
                                }
                              """
                              end
                              end
                           ,
                           points
                         )
                        )
                      end
                   }
                   
                   ${if listLength(sparse) == 0 || isAllCond(p.conds)
                     then 
                       implode("\n",
                         map(
                           \ si::Pair<String Integer>
                           -> let Dj::String = s"${si.fst}${toString(si.snd)}"
                           in s"if(${iv}${si.fst} == ${iv}) p${Dj}++;"
                           end
                           ,
                           sparse
                         )
                       )
                       ++ s"${iv}++;"
                     else if listLength(sparse) == 1 && !allBelow
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
                       )
                   }
                 }
               """
               end
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
String ::= exprs::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] order::[String] loc::Location env::Decorated Env
{
  local expr::TensorAssignExpr =
    head(exprs).fst.fst;
  local subs::[Pair<TensorExpr String>] =
    head(exprs).fst.snd;
  local endExpr::TensorAssignExpr =
    head(exprs).snd.fst;
  
  local decl::[String] = 
    if null(exprs) || null(tail(exprs))
    then []
    else 
      map(
        \ p::Pair<TensorExpr String> 
        -> p.snd
        ,
        head(tail(exprs)).fst.snd
      )
      ++
      map(
        \ p::Pair<TensorExpr String>
        -> p.snd
        ,
        head(tail(exprs)).snd.snd
      );

  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);

  local iv::String = head(order);

  local out::String =
    case expr.tensorAssign of
    | access(n, _) -> n.name
    | _ -> ""
    end;
  local outLen::Integer =
    case expr.tensorAssign of
    | access(_, l) -> listLength(l)
    | _ -> -1
    end;
  local lastSparse::Boolean =
    case tm:lookup(name(out, location=loc), expr.tensorFormat) of
    | f::[] -> 
        let idx::Integer = last(f.dimenOrder)
        in
        case getElem(f.specifiers, idx) of
        | nothing() -> false
        | just(t) -> t == storeSparse
        end
        end
    | _ -> false
    end;
  local out_sparse::Boolean =
    case sparse_assign(expr, iv) of
    | si::[] ->
        let f::TensorFormatItem =
          head(tm:lookup(name(si.fst, location=loc), expr.tensorFormat))
        in
        si.snd != f.dimens
        end
    | _ -> false
    end;
  local out_is_sparse::Boolean =
    case sparse_assign(expr, iv) of
    | si::[] -> true
    | _ -> false
    end;
  local out_acc::Integer =
    case sparse_assign(expr, iv) of
    | si::[] -> si.snd
    | _ -> -1
    end;
  local is_out_last::Boolean =
    case expr.tensorAssign of
    | access(_, acc) ->
        !containsAny(
          stringEq(_, _),
          tail(order),
          acc
        )
        &&
        containsBy(
          stringEq(_, _),
          iv,
          acc
        )
    | _ -> false
    end;
  local is_inside::Boolean =
    case expr.tensorAssign of
    | access(_, acc) ->
        !containsAny(
          stringEq(_, _),
          order,
          acc
        )
    | _ -> false
    end;

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
    unsigned long ${iv} = 0;
    ${implode("\n",
      map(
        \ p::LatticePoints
       -> 
          let sparse::[Pair<String Integer>] =
            sparse_dimensions(builtLattice([p]), iv)
          in let dense::[Pair<String Integer>] =
            dense_dimensions(builtLattice([p]), iv)
          in let allBelow::Boolean =
            containsFunc(
              \ lp::LatticePoints
              -> isAllCond(lp.conds)
              ,
              p.points
            )
          in
          s"""
          while(${until_any_exhausted(merged_dimensions(p, expr.tensorFormat), expr, iv)}${if isAllCond(p.conds) && !null(sparse) then s" && ${implode("&&", map(\p::Pair<String Integer> -> s"p${p.fst}${toString(p.snd)} < ${p.fst}${toString(p.snd)}_pos[p${p.fst}${toString(p.snd-1)}+1]", sparse))}" else ""}) {
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
            
            ${if !null(sparse) && !isAllCond(p.conds)
              then s"${iv} = ${generate_min(map(\si::Pair<String Integer> -> s"${iv}${si.fst}", sparse))};"
              else s""
            }
            
            ${implode("\n",
              map(
                \si::Pair<String Integer> ->
                  let Dj::String = s"${si.fst}${toString(si.snd)}"
                  in let Dj1::String = s"${si.fst}${toString(si.snd - 1)}"
                  in
                  s"""
                    unsigned long p${Dj} = (p${Dj1} * ${Dj}_size) + ${iv};
                  """
                  end end
                ,
                dense
              )
            )}
            
            ${if out_is_sparse
              then 
                let a::String = toString(out_acc)
                in
                s"""
                  while(${out}${a}_idx[p${out}${a}] < ${iv}) {
                    p${out}${a}++;
                  }
                """
                end
              else ""
            }
                        
            // section 6.2, emit-available-expressions
            /*${emitAvailExprs(expr, subs, decl, iv, is_inside, is_out_last, null(tail(order)), env)}*/
            //
            
            if(0) {}
            
            ${let points::[LatticePoints] =
                (if isAllCond(p.conds) && !null(sub_points(p)) then [] else [p]) ++ sub_points(p)
              in
              if listLength(points) == 0 || isNullCond(head(points).conds)
              then 
                let pnt::LatticePoints =
                  head(points)
                in
                let ex::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
                  exprSubs(pnt.exprs, order)
                in
                s"""
                  else {
                    ${emitAvailExprs(head(ex).fst.fst, head(ex).fst.snd, decl, iv, is_inside, is_out_last, null(tail(order)), env)}
                    //
                    
                    ${code_gen(tail(ex), tail(order), loc, env)}
                    
                    // section 6.2, emit-reduction-compute
                    ${emitReduceCompute(head(ex).snd.fst, head(ex).snd.snd, iv, is_inside, env)}
                    // section 6.2, emit-compute
                    ${emitCompute(head(ex).snd.fst, head(ex).snd.snd, iv, is_out_last, env)}
                  }
                """
                end
                end
              else 
                implode("\n",
                  map(
                    \ pnt::LatticePoints
                    -> let sd::[Pair<String Integer>] = sparse_dimensions(builtLattice([pnt]), iv)
                       in
                       let ex::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
                         exprSubs(pnt.exprs, order)
                       in
                       s"""
                          ${if null(sd)
                            then s"else {"
                            else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}){"
                          }
                     
                          ${emitAvailExprs(head(ex).fst.fst, head(ex).fst.snd, decl, iv, is_inside, is_out_last, null(tail(order)), env)}
                          //
                     
                          ${code_gen(tail(ex), tail(order), loc, env)}
                          
                          // #7, emit-reduction-compute
                          ${emitReduceCompute(head(ex).snd.fst, head(ex).snd.snd, iv, is_inside, env)}
                          // #7, emit-compute()
                          ${emitCompute(head(ex).snd.fst, head(ex).snd.snd, iv, is_out_last, env)}
                          }
                       """
                       end
                       end
                    ,
                    points
                  )
                )
              end
            }
            
            ${let points::[LatticePoints] =
                p :: sub_points(p)
              in
              if listLength(points) == 0
              || isNullCond(head(points).conds)
              || isAllCond(head(points).conds)
              || foldl(
                   \ b::Boolean
                     pnt::LatticePoints
                   -> b 
                   || null(sparse_dimensions(builtLattice([pnt]), iv))
                   || !is_out_last
                   ,
                   false,
                   points
                 )
              then ""
              else if lastSparse
              then 
                let var::String = 
                  s"${out}${toString(outLen)}"
                in
                let v1::String =
                  s"${out}${toString(outLen - 1)}"
                in
                s"""
                  else if(${var}_idx[p${var}] <= ${iv} && p${var} < ${var}_pos[p${v1} + 1]){
                    while(${var}_idx[p${var}] <= ${iv} && p${var} < ${var}_pos[p${v1} + 1]) {
                      p${var}++;
                    }
                  }
                """
                end
                end
              else ""
              end
            }
            
            ${if listLength(sparse) == 0 || isAllCond(p.conds)
              then
                implode("\n",
                  map(
                    \ si::Pair<String Integer>
                    -> let Dj::String = s"${si.fst}${toString(si.snd)}"
                       in s"if(${iv}${si.fst} == ${iv}) p${Dj}++;"
                       end
                    ,
                    sparse
                  )
                )
                ++ s"${iv}++;"
              else if listLength(sparse) == 1 && !allBelow
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
            ${if out_sparse
              then s"p${out}${toString(out_acc)}++;"
              else ""
            }
          }          
        """
        end
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

function isSparseNext
Boolean ::= tensor::String dim::Integer fmts::tm:Map<Name TensorFormatItem> loc::Location
{
  local fmt::TensorFormatItem =
    head(tm:lookup(name(tensor, location=loc), fmts));
  
  local idx::Integer =
    positionOf(
      \ i1::Integer
        i2::Integer
      -> i1 == i2
      ,
      dim - 1,
      fmt.dimenOrder
    );
  
  local dN::Integer =
    case getElem(fmt.dimenOrder, idx+1) of
    | nothing() -> -1
    | just(x) -> x
    end;
  
  local form::Integer =
    case getElem(fmt.specifiers, dN) of
    | nothing() -> storeDense
    | just(x) -> x
    end;
  
  return dN != -1 && form == storeSparse;
}

function previousDense
Boolean ::= tensor::String dim::Integer fmts::tm:Map<Name TensorFormatItem> loc::Location
{
  local fmt::TensorFormatItem =
    head(tm:lookup(name(tensor, location=loc), fmts));
  
  local idx::Integer =
    positionOf(
      \ i1::Integer
        i2::Integer
      -> i1 == i2
      ,
      dim - 1,
      fmt.dimenOrder
    );
  
  local dP::Integer =
    case getElem(fmt.dimenOrder, idx-1) of
    | nothing() -> -1
    | just(x) -> x
    end;
  
  local form::Integer =
    case getElem(fmt.specifiers, dP) of
    | nothing() -> storeSparse
    | just(x) -> x
    end;
  
  return dP != -1 && form == storeDense;
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

function displaySub
String ::= p::Pair<TensorExpr String>
{
  return s"(${show(100, p.fst.pp)} -> ${p.snd})";
}

function emitAvailExprs
String ::= ex::TensorAssignExpr subs::[Pair<TensorExpr String>] decl::[String] iv::String inside::Boolean layer::Boolean last::Boolean env::Decorated Env
{
  return
    (if !last
    then
      implode("\n",
        map(
          \ p::Pair<TensorExpr String>
          -> s"double ${p.snd} = ${evalExpr(p.fst, env)};"
          ,
          subs
        )
      )
    else 
      implode("\n",
        map(
          \ p::Pair<TensorExpr String>
          -> s"${p.snd} += ${evalExpr(p.fst, env)};"
          ,
          subs
        )
      )
    )
    ++
    if inside || layer
    then
      implode("\n",
        map(
          \ s::String
          -> s"double ${s} = 0.0;"
          ,
          decl
        )
      )
    else "";
}

function emitReduceCompute
String ::= ex::TensorAssignExpr sbs::[Pair<TensorExpr String>] iv::String inside::Boolean env::Decorated Env
{
  return
    if inside
    then
    implode("\n",
      map(
        \ p::Pair<TensorExpr String> 
        -> s"${p.snd} += ${evalExpr(p.fst, env)};"
        ,
        sbs
      )
    )
    else "";
}

function emitCompute
String ::= ex::TensorAssignExpr subs::[Pair<TensorExpr String>] iv::String layer::Boolean env::Decorated Env
{
  local out::String =
    case ex.tensorAssign of
    | access(o, _) -> o.name
    | _ -> "error"
    end;
  
  local outAccess::String =
    case ex.tensorAssign of
    | access(o, acc) -> s"p${o.name}${toString(listLength(acc))}"
    | _ -> "error"
    end;

  local error::Boolean =
    layer
    &&
    listLength(subs) != 1;

  return
    if listLength(subs) > 1
    then s"""fprintf(stderr, "Parsing error...");"""
    else
    if listLength(subs) == 0
    then s"${out}->data[${outAccess}] += ${evalExpr(ex.tensorValue, env)};"
    else
    if layer
    then
    s"${out}->data[${outAccess}] += ${evalExpr(head(subs).fst, env)};"
    else "";
}

--function deeperSubs
--[Pair<TensorExpr String>] ::= ex::TensorExpr left::[String]
--{
--  return
--    deeperSubs_helper(ex, reverse(left), []);
--}

--function deeperSubs_helper
--[Pair<TensorExpr String>] ::= ex::TensorExpr vars::[String] before::[String]
--{
--  return
--    if null(vars)
--    then []
--    else 
--      let subs::[Pair<TensorExpr String>] =
--        findSubs(ex, head(vars), before)
--      in
--      deeperSubs_helper(exprSub(ex, subs), tail(vars), head(vars) :: before)
--      ++ 
--      subs
--      end;
--
--}

--function findDeeperSubs
--[String] ::= ex::TensorExpr left::[String]
--{
--  return
--    map(
--      \ p::Pair<TensorExpr String>
--      -> p.snd
--      ,
--      deeperSubs(ex, left)
--    );
--}

function makeSub
TensorAssignExpr ::= ex::TensorAssignExpr sub::[Pair<TensorExpr String>]
{
  local subd::TensorExpr =
    exprSub(ex.tensorValue, sub);

  return 
    assignExprExpr(
      ex.tensorAssign,
      subd,
      ex.tensorFormat,
      location = ex.location
    );
}

function exprSub
TensorExpr ::= ex::TensorExpr subs::[Pair<TensorExpr String>]
{
  return
    case ex of
    | nullTensorExpr() -> ex
    | access(_, _) ->
        let idx::Integer =
          positionOf(
            tensorExprEqual(_, _),
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              , 
              subs
            )
          )
        in
        if idx == -1
        then ex
        else 
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in
          tExpr(
            declRefExpr(
              name(str, location=ex.location),
              location=ex.location
            ),
            location=ex.location
          )
          end
          end
        end
    | tExpr(declRefExpr(name(s))) ->
        let idx::Integer =
          positionOf(
            \ e1::TensorExpr
              e2::TensorExpr
            -> case e2 of
               | tExpr(declRefExpr(name(str))) ->
                   s == str
               | _ -> false
               end
            ,
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              ,
              subs
            )
          )
        in 
        if idx == -1
        then ex
        else
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in
          tExpr(
            declRefExpr(
              name(str, location=ex.location),
              location=ex.location
            ),
            location=ex.location
          )
          end
          end
        end
    | tExpr(_) -> ex
    | add(l, r) -> 
        let idx::Integer =
          positionOf(
            tensorExprEqual(_, _),
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              , 
              subs
            )
          )
        in
        if idx == -1
        then add(exprSub(l, subs), exprSub(r, subs), location=ex.location)
        else 
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in
          tExpr(
            declRefExpr(
              name(str, location=ex.location),
              location=ex.location
            ),
            location=ex.location
          )
          end
          end
        end
    | sub(l, r) ->
        let idx::Integer =
          positionOf(
            tensorExprEqual(_, _),
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              ,
              subs
            )
          )
        in
        if idx == -1
        then sub(exprSub(l, subs), exprSub(r, subs), location=ex.location)
        else
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in
          tExpr(
            declRefExpr(
              name(str, location=ex.location),
              location=ex.location
            ),
            location=ex.location
          )
          end
          end
        end
    | mul(l, r) ->
        let idx::Integer =
          positionOf(
            tensorExprEqual(_, _),
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              , 
              subs
            )
          )
        in
        if idx == -1
        then mul(exprSub(l, subs), exprSub(r, subs), location=ex.location)
        else 
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in 
          tExpr(
            declRefExpr(
              name(str, location=ex.location), 
              location=ex.location
            ), 
            location=ex.location
          )
          end
          end
        end
    | div(l, r) -> 
        let idx::Integer =
          positionOf(
            tensorExprEqual(_, _),
            ex,
            map(
              \ p::Pair<TensorExpr String>
              -> p.fst
              ,
              subs
            )
          )
        in
        if idx == -1
        then div(exprSub(l, subs), exprSub(r, subs), location=ex.location)
        else
          let res::Maybe<Pair<TensorExpr String>>
            = getElem(subs, idx)
          in
          let str::String =
            case res of
            | nothing() -> "error"
            | just(p) -> p.snd
            end
          in
          tExpr(
            declRefExpr(
              name(str, location=ex.location),
              location=ex.location
            ),
            location = ex.location
          )
          end
          end
        end
    end;
}

--function findSubs
--[Pair<TensorExpr String>] ::= ex::TensorExpr iv::String left::[String]
--{
--  return findSubs_helper(ex, iv, left, 0).fst;
--}

--function findSubs_helper
--Pair<[Pair<TensorExpr String>] Integer> ::= ex::TensorExpr iv::String left::[String] c::Integer
--{
--  return
--    case ex of
--    | nullTensorExpr() -> pair([], c)
--    | access(_, acc) ->
--        if !containsAny(
--             stringEq(_, _),
--             left,
--             acc
--           )
--           &&
--           containsBy(
--             stringEq(_, _),
--             iv,
--             acc
--           )
--        then pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
--        else pair([], c)
--    | tExpr(_) -> pair([], c)
--    | add(l, r) ->
--        if isAvail(ex, left, iv)
--        then 
--          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
--        else
--          let pl::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(l, iv, left, c)
--          in
--          let pr::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(r, iv, left, pl.snd)
--          in
--          pair(pl.fst ++ pr.fst, pr.snd)
--          end
--          end
--    | sub(l, r) ->
--        if isAvail(ex, left, iv)
--        then
--          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
--        else
--          let pl::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(l, iv, left, c)
--          in
--          let pr::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(r, iv, left, pl.snd)
--          in
--          pair(pl.fst ++ pr.fst, pr.snd)
--          end
--          end
--    | mul(l, r) ->
--        if isAvail(ex, left, iv)
--        then
--          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
--        else
--          let pl::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(l, iv, left, c)
--          in
--          let pr::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(r, iv, left, pl.snd)
--          in
--          pair(pl.fst ++ pr.fst, pr.snd)
--          end
--          end
--    | div(l, r) ->
--        if isAvail(ex, left, iv)
--        then
--          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
--        else
--          let pl::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(l, iv, left, c)
--          in
--          let pr::Pair<[Pair<TensorExpr String>] Integer> =
--            findSubs_helper(r, iv, left, pl.snd)
--          in
--          pair(pl.fst ++ pr.fst, pr.snd)
--          end
--          end
--    end;
--}

function isAvail
Boolean ::= ex::TensorExpr left::[String] iv::String isOr::Boolean isOut::Boolean
{
  return
    case ex of
    | nullTensorExpr() -> true
    | access(_, acc) ->
        !containsAny(
          stringEq(_, _),
          left,
          acc
         ) 
         &&
         containsBy(
           stringEq(_, _),
           iv,
           acc
         )
    | tExpr(declRefExpr(name(s))) ->
        let v::String =
          parseVar(s)
        in
          isOut || !isOr || length(v) == 0 ||
          containsBy(
            stringEq(_, _),
            v,
            iv::left
          )
        end
    | tExpr(_) -> true
    | add(l, r) -> isAvail(l, left, iv, true, isOut) && isAvail(r, left, iv, true, isOut)
    | sub(l, r) -> isAvail(l, left, iv, true, isOut) && isAvail(r, left, iv, true, isOut)
    | mul(l, r) -> isAvail(l, left, iv, false, isOut) && isAvail(r, left, iv, false, isOut)
    | div(l, r) -> isAvail(l, left, iv, false, isOut) && isAvail(r, left, iv, false, isOut)
    end;
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
String ::= e::TensorExpr env::Decorated Env
{
  e.env = env;

  return
    case e of
    | nullTensorExpr() -> ""
    | access(nm, acc) -> s"${nm.name}->data[p${nm.name}${toString(listLength(acc))}]"
    | tExpr(declRefExpr(name(s))) -> s
    | tExpr(expr) -> 
        s"_expr_${toString(expr.location.line)}_${toString(expr.location.column)}"
    | add(l, r) -> s"(${evalExpr(l, env)} + ${evalExpr(r, env)})"
    | sub(l, r) -> s"(${evalExpr(l, env)} - ${evalExpr(r, env)})"
    | mul(l, r) -> s"(${evalExpr(l, env)} * ${evalExpr(r, env)})"
    | div(l, r) -> s"(${evalExpr(l, env)} / ${evalExpr(r, env)})"
    end;
}


function exprSubs
[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] 
  ::= expr::TensorAssignExpr access::[String]
{
  local tmap::tm:Map<String Integer> =
    tm:add(
      map(
        \s::String -> pair(s, 0),
        access
      ),
      tm:empty(compareString(_, _))
    );
  
  local res::[Pair<TensorAssignExpr [Pair<TensorExpr String>]>] =
    exprSubs_helper(expr, access, tmap);

  local pres::[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>>] =
    zipWith(
      pair(_, _),
      take(listLength(access), res),
      reverse(drop(listLength(access), res))
    );
  
  return
    if listLength(pres) == 1
    then [pair(pair(expr, []), pair(expr, [head(head(pres).fst.snd)]))]
    else pres;
}

function exprSubs_helper
[Pair<TensorAssignExpr [Pair<TensorExpr String>]>] ::= expr::TensorAssignExpr left::[String] tmap::tm:Map<String Integer>
{
  local acc::[String] =
    case expr.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;

  local isOut::Boolean =
    !containsAny(
      stringEq(_, _),
      tail(left),
      acc
    )
    &&
    containsBy(
      stringEq(_, _),
      head(left),
      acc
    );

  return
    if null(left)
    then pair(expr, []) :: pair(expr, []) :: []
    else
      let subs::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
        findSubs_new(expr.tensorValue, head(left), tail(left), false, isOut, tmap)
      in
      let inner::[Pair<TensorAssignExpr [Pair<TensorExpr String>]>] =
        exprSubs_helper(
          makeSub(expr, subs.fst),
          tail(left),
          subs.snd
        )
      in
      let nSubs::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
        findSubs_new(last(inner).fst.tensorValue, head(left), left, false, isOut, subs.snd)
      in
      let newExpr::TensorAssignExpr =
        makeSub(last(inner).fst, nSubs.fst)
      in
      pair(expr, subs.fst) :: inner ++ [pair(newExpr, nSubs.fst)]
      end
      end
      end
      end;
}

function findSubs_new
Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> ::= expr::TensorExpr iv::String left::[String] isOr::Boolean isOut::Boolean map::tm:Map<String Integer>
{
  return
    case expr of
    | nullTensorExpr() -> pair([], map)
    | tExpr(declRefExpr(name(str)))
      -> if !startsWith("t", str)
         then pair([], map)
         else
         let i::String =
           parseVar(substring(1, length(str), str))
         in
         if (!isOr || isOut) && i != iv && length(i) > 0
         then 
           let num::Integer =
             head(tm:lookup(iv, map))
           in
           let nMap::tm:Map<String Integer> =
             tm:update(iv, [num+1], map)
           in
             pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
           end
           end
         else pair([], map)
         end
    | tExpr(_) -> pair([], map)
    | access(nm, acc) -> 
        if !containsAny(
             stringEq(_, _),
             left,
             acc
           ) &&
           containsBy(
             stringEq(_, _),
             iv,
             acc
           )
        then
          let num::Integer =
            head(tm:lookup(iv, map))
          in
          let nMap::tm:Map<String Integer> =
            tm:update(iv, [num+1], map)
          in
            pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
          end
          end
        else pair([], map)
    | add(l, r) -> 
        if isAvail(expr, left, iv, isOr, isOut)
        then
          let num::Integer =
           head(tm:lookup(iv, map))
          in
          let nMap::tm:Map<String Integer> =
            tm:update(iv, [num+1], map)
          in
            pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
          end
          end
        else 
          let lres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(l, iv, left, true, isOut, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, true, isOut, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end
    | sub(l, r) -> 
        if isAvail(expr, left, iv, isOr, isOut)
        then
          let num::Integer =
            head(tm:lookup(iv, map))
          in
          let nMap::tm:Map<String Integer> =
            tm:update(iv, [num+1], map)
          in
            pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
          end
          end
        else
          let lres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(l, iv, left, true, isOut, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, true, isOut, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end
    | mul(l, r) -> 
        if isAvail(expr, left, iv, isOr, isOut)
        then
          let num::Integer =
            head(tm:lookup(iv, map))
          in
          let nMap::tm:Map<String Integer> =
            tm:update(iv, [num+1], map)
          in
            pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
          end
          end
        else
          let lres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(l, iv, left, false, isOut, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, false, isOut, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end        
    | div(l, r) -> 
        if isAvail(expr, left, iv, isOr, isOut)
        then
          let num::Integer =
            head(tm:lookup(iv, map))
          in
          let nMap::tm:Map<String Integer> =
            tm:update(iv, [num+1], map)
          in
            pair([pair(expr, s"t${iv}${toString(num)}")], nMap)
          end
          end
        else
          let lres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(l, iv, left, false, isOut, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, false, isOut, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end        
    end;
}

function parseVar
String ::= var::String
{
  return parseVar_helper(var, "");
}

function parseVar_helper
String ::= nm::String var::String
{
  return
    if length(nm) == 0
    then ""
    else 
      if isAlpha(substring(0, 1, nm))
      then parseVar_helper(substring(1, length(nm), nm), var ++ substring(0, 1, nm))
      else 
        case toIntSafe(nm) of
        | nothing() -> ""
        | just(_) -> var
        end;
}

function writeExprSub
String ::= pr::Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> Pair<TensorAssignExpr [Pair<TensorExpr String>]>> env::Decorated Env
{
  return
    s"""${evalExpr(pr.fst.fst.tensorValue, env)} (${implode(", ", map(\p::Pair<TensorExpr String> -> s"${evalExpr(p.fst, env)} -> ${p.snd}", pr.fst.snd))}) -- ${evalExpr(pr.snd.fst.tensorValue, env)} (${implode(", ", map(\p::Pair<TensorExpr String> -> s"${evalExpr(p.fst, env)} -> ${p.snd}", pr.snd.snd))})""";
}
