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
  
  local fwrd::Stmt =
    substStmt(
      generateExprSubs(exprs, exprN),
      parseStmt(s"""
        {
          ${check_dims(assign, acc)}
          ${pack_tensors(tail(tensors), tail(formats))}
          ${setup_gen(tensors, formats)}
          ${build_output(assign, acc, tensors, loc)}
          {
            ${setup_gen(head(tensors) :: [], head(formats) :: [])}
            ${code_gen(assign, acc, loc, [], env)}
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
String ::= expr::TensorAssignExpr order::[String] tensors::[Name] loc::Location
{
  local nm::Name =
    case expr.tensorAssign of
    | access(n, _) -> n
    | _ -> name("error", location=loc)
    end;
  local out::String = nm.name;
  local fmt::TensorFormatItem =
    head(tm:lookup(nm, expr.tensorFormat));

  local ex::TensorExpr = expr.tensorValue;
  ex.parenExpr = [];

  return s"""
  {
    if(${implode("||", map(\n::Name -> s"""strcmp(${n.name}->form, "${ex.proceduralName}") != 0""", tensors))} ) {    
      ${out}->bufferCnt = 0;
      ${out}->buffer.numChildren = 0;
      ${out}->buffer.children = 0;
      
      unsigned long* index = GC_malloc(sizeof(unsigned long) * ${toString(fmt.dimens)});
      ${build_body(expr, order, loc)}
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
String ::= expr::TensorAssignExpr order::[String] loc::Location
{
  local nm::Name =
    case expr.tensorAssign of
    | access(n, _) -> n
    | _ -> name("error", location=loc)
    end;

  local out::String = nm.name;

  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);
  
  local fmt::TensorFormatItem =
    head(tm:lookup(nm, expr.tensorFormat));
  
  local acc::[String] =
    case expr.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
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
                   
                   ${if !null(sparse) && !allBelow
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
                       p :: sub_points(p)
                     in
                     if listLength(points) == 0 || isAccessCond(head(points).conds) ||
                        isNullCond(head(points).conds)
                     then
                       let pnt::LatticePoints =
                         head(points)
                       in
                       s"""
                         else {
                           ${build_body(pnt.exprs, tail(order), loc)}
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
                                  then s"else {"
                                  else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}) {"
                                }
                                
                                ${build_body(pnt.exprs, tail(order), loc)}
                                
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
                   ${if allBelow
                     then s"${iv}++;"
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
String ::= expr::TensorAssignExpr order::[String] loc::Location subs::[Pair<TensorExpr String>] env::Decorated Env
{
  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);

  local iv::String = head(order);

  local nSubs::[Pair<TensorExpr String>] =
    findSubs(exprSub(expr.tensorValue, subs), iv, tail(order));

  local subExpr::TensorAssignExpr =
    makeSub(expr, nSubs ++ subs);

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
            
            ${if !null(sparse) && !allBelow
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
            ${emitAvailExprs(expr, iv, tail(order), env)}
            
            if(0) {}
            
            ${let points::[LatticePoints] =
                p :: sub_points(p)
              in
              if listLength(points) == 0 || isNullCond(head(points).conds) ||
                 isAllCond(head(points).conds)
              then 
                let pnt::LatticePoints =
                  head(points)
                in
                let nxtExpr::TensorAssignExpr =
                  makeSub(pnt.exprs, subs)
                in
                let nExpr::TensorAssignExpr =
                  case tail(order) of
                  | [] -> nxtExpr
                  | _ -> makeSub(nxtExpr, nSubs)
                  end
                in
                s"""
                  else {
                    ${code_gen(pnt.exprs, tail(order), loc, nSubs ++ subs, env)}

                    
                    // section 6.2, emit-reduction-compute
                    // pnt.exprs was nxtExpr
                    ${emitReduceCompute(nxtExpr, iv, tail(order), nSubs ++ subs, env)}
                    // section 6.2, emit-compute
                    ${emitCompute(nExpr, iv, tail(order), subs, env)}
                  }
                """
                end
                end
                end
              else 
                implode("\n",
                  map(
                    \ pnt::LatticePoints
                    -> let sd::[Pair<String Integer>] = sparse_dimensions(builtLattice([pnt]), iv)
                       in
                       let nxtExpr::TensorAssignExpr =
                         makeSub(pnt.exprs, subs)
                       in
                       let nExpr::TensorAssignExpr =
                         case tail(order) of
                         | [] -> nxtExpr
                         | _ -> makeSub(nxtExpr, nSubs)
                         end
                       in
                       s"""
                          ${if null(sd)
                            then s"else {"
                            else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}){"
                          }
                     
                          ${code_gen(pnt.exprs, tail(order), loc, nSubs ++ subs, env)}
                          
                          // #7, emit-reduction-compute
                          // pnt.exprs was nxtExpr
                          ${emitReduceCompute(nxtExpr, iv, tail(order), subs, env)}
                          // #7, emit-compute()
                          ${emitCompute(nExpr, iv, tail(order), subs, env)}
                          }
                       """
                       end
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
                   || emitCompute(pnt.exprs, iv, tail(order), subs, env) == ""  
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
            
            ${if listLength(sparse) == 0
              then s"${iv}++;"
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
            ${if allBelow
              then s"${iv}++;"
              else ""
            }
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
String ::= ex::TensorAssignExpr iv::String left::[String] env::Decorated Env
{
  local subs::[Pair<TensorExpr String>] =
    findSubs(ex.tensorValue, iv, left);
  
  local later::[String] =
    map(
      \p::Pair<TensorExpr String> -> p.snd
      , 
      findSubs(
        ex.tensorValue, 
        head(left), 
        tail(left)
      )
    );
  
  local last::Boolean = null(left);
  
  return
    if !last
    then
      implode("\n",
        map(
          \ p::Pair<TensorExpr String> ->
          s"double ${p.snd} = ${evalExpr(p.fst, env)};"
          ,
          subs
        )
      )
      ++
      implode("\n",
        map(
          \ s::String ->
          s"double ${s} = 0.0;"
          ,
          later
        )
      )
    else "";
}

function emitReduceCompute
String ::= ex::TensorAssignExpr iv::String left::[String] sbs::[Pair<TensorExpr String>] env::Decorated Env
{
  local exs::TensorAssignExpr =
    makeSub(ex, sbs);
  local expr::TensorAssignExpr =
    makeSub(exs, deeperSubs(exs.tensorValue, left));
  local subs::[Pair<TensorExpr String>] =
    findSubs(expr.tensorValue, iv, left);

  local acc::[String] =
    case ex.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
  local sub::Boolean =
    !containsAny(
       stringEq(_, _),
       iv :: left,
       acc
     );
  
  return
    (if sub
    then
      implode("\n",
        map(
          \ p::Pair<TensorExpr String> ->
          s"${p.snd} += ${evalExpr(p.fst, env)};"
          ,
          subs
      ))
    else "")
    ++
    (if null(subs) && sub
    then s"""
      t${iv}${toString(getNumber(sbs, iv) + 1)} += ${evalExpr(expr.tensorValue, env)};
    """
    else "");
}

function getNumber
Integer ::= sbs::[Pair<TensorExpr String>] iv::String
{
  return
    if null(sbs)
    then -1
    else 
      let this::Integer =
        indexOf(iv, substring(1, length(head(sbs).snd), head(sbs).snd))
      in
      max(
        this,
        getNumber(tail(sbs), iv)
      )
      end;
}

function emitCompute
String ::= ex::TensorAssignExpr iv::String left::[String] sbs::[Pair<TensorExpr String>] env::Decorated Env
{
  local eAssign::TensorExpr =
    ex.tensorAssign;
  eAssign.env = env;

  local expr::TensorAssignExpr =
    makeSub(ex, deeperSubs(ex.tensorValue, left) ++ sbs);

  local subs::[Pair<TensorExpr String>] =
    findSubs(expr.tensorValue, if null(left) then "" else head(left), []);

  local exp::TensorAssignExpr =
    makeSub(expr, subs);

  local acc::[String] =
    case eAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
  local out::String =
    case eAssign of
    | access(n, _) -> n.name
    | _ -> "error"
    end;
  
  local layer::Boolean =
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
    );

  local sparse::Boolean =
    case eAssign of
    | access(n, _) ->
        let f::TensorFormatItem =
          getFormat(n, env)
        in
        let idx::Integer =
          last(f.dimenOrder)
        in
        case getElem(f.specifiers, idx) of
        | nothing() -> false
        | just(x) -> x == storeSparse
        end
        end
        end
    | _ -> false
    end;  

  return
    if layer
    then
      s"""
        ${out}->data[p${out}${toString(listLength(acc))}] += ${evalExpr(exp.tensorValue, env)};
        ${if sparse
          then s"p${out}${toString(listLength(acc))}++;"
          else ""
        }
      """
    else "";
}

function deeperSubs
[Pair<TensorExpr String>] ::= ex::TensorExpr left::[String]
{
  return
    deeperSubs_helper(ex, reverse(left), []);
}

function deeperSubs_helper
[Pair<TensorExpr String>] ::= ex::TensorExpr vars::[String] before::[String]
{
  return
    if null(vars)
    then []
    else 
      let subs::[Pair<TensorExpr String>] =
        findSubs(ex, head(vars), before)
      in
      deeperSubs_helper(exprSub(ex, subs), tail(vars), head(vars) :: before)
      ++ 
      subs
      end;

}

function findDeeperSubs
[String] ::= ex::TensorExpr left::[String]
{
  return
    map(
      \ p::Pair<TensorExpr String>
      -> p.snd
      ,
      deeperSubs(ex, left)
    );
}

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

function findSubs
[Pair<TensorExpr String>] ::= ex::TensorExpr iv::String left::[String]
{
  return findSubs_helper(ex, iv, left, 0).fst;
}

function findSubs_helper
Pair<[Pair<TensorExpr String>] Integer> ::= ex::TensorExpr iv::String left::[String] c::Integer
{
  return
    case ex of
    | nullTensorExpr() -> pair([], c)
    | access(_, acc) ->
        if !containsAny(
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
        then pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else pair([], c)
    | tExpr(_) -> pair([], c)
    | add(l, r) ->
        if isAvail(ex, left, iv)
        then 
          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else
          let pl::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(l, iv, left, c)
          in
          let pr::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(r, iv, left, pl.snd)
          in
          pair(pl.fst ++ pr.fst, pr.snd)
          end
          end
    | sub(l, r) ->
        if isAvail(ex, left, iv)
        then
          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else
          let pl::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(l, iv, left, c)
          in
          let pr::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(r, iv, left, pl.snd)
          in
          pair(pl.fst ++ pr.fst, pr.snd)
          end
          end
    | mul(l, r) ->
        if isAvail(ex, left, iv)
        then
          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else
          let pl::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(l, iv, left, c)
          in
          let pr::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(r, iv, left, pl.snd)
          in
          pair(pl.fst ++ pr.fst, pr.snd)
          end
          end
    | div(l, r) ->
        if isAvail(ex, left, iv)
        then
          pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else
          let pl::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(l, iv, left, c)
          in
          let pr::Pair<[Pair<TensorExpr String>] Integer> =
            findSubs_helper(r, iv, left, pl.snd)
          in
          pair(pl.fst ++ pr.fst, pr.snd)
          end
          end
    end;
}

function isAvail
Boolean ::= ex::TensorExpr left::[String] iv::String
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
    | tExpr(_) -> true
    | add(l, r) -> isAvail(l, left, iv) && isAvail(r, left, iv)
    | sub(l, r) -> isAvail(l, left, iv) && isAvail(r, left, iv)
    | mul(l, r) -> isAvail(l, left, iv) && isAvail(r, left, iv)
    | div(l, r) -> isAvail(l, left, iv) && isAvail(r, left, iv)
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
[Pair<Pair<TensorAssignExpr [Pair<TensorExpr String>]> TensorAssignExpr>] 
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
  
  return
    zipWith(
      pair(_, _),
      take(listLength(access), res),
      map(
        \p::Pair<TensorAssignExpr [Pair<TensorExpr String>]> 
        -> p.fst
        , 
        reverse(drop(listLength(access), res))
      )
    );
}

function exprSubs_helper
[Pair<TensorAssignExpr [Pair<TensorExpr String>]>] ::= expr::TensorAssignExpr left::[String] tmap::tm:Map<String Integer>
{
  return
    if null(left)
    then pair(expr, []) :: pair(expr, []) :: []
    else
      let subs::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
        findSubs_new(expr.tensorValue, head(left), tail(left), tmap)
      in
      let inner::[Pair<TensorAssignExpr [Pair<TensorExpr String>]>] =
        exprSubs_helper(
          makeSub(expr, subs.fst),
          tail(left),
          subs.snd
        )
      in
      let nSubs::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
        findSubs_new(expr.tensorValue, head(left), [], tmap)
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
Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> ::= expr::TensorExpr iv::String left::[String] map::tm:Map<String Integer>
{
  return
    case expr of
    | nullTensorExpr() -> pair([], map)
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
        if isAvail(expr, left, iv)
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
            findSubs_new(l, iv, left, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end
    | sub(l, r) -> 
        if isAvail(expr, left, iv)
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
            findSubs_new(l, iv, left, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end
    | mul(l, r) -> 
        if isAvail(expr, left, iv)
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
            findSubs_new(l, iv, left, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end        
    | div(l, r) -> 
        if isAvail(expr, left, iv)
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
            findSubs_new(l, iv, left, map)
          in
          let rres::Pair<[Pair<TensorExpr String>] tm:Map<String Integer>> =
            findSubs_new(r, iv, left, lres.snd)
          in
          pair(rres.fst ++ lres.fst, rres.snd)
          end
          end        
    end;
}
