grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function codeGen
Stmt ::= nm::Name access::[String] expr::TensorExpr env::Decorated Env loc::Location
{
  local tensors::[Name] = nm :: getTensors(expr);
  local formats::[TensorFormatItem] = getFormats(tensors, env);

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
    [err(loc, s"Cannot generate code for this tensor calculation due to cyclical access pattern.")];

  return if null(acc)
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

  return s"""
  {
    if(${implode("||", map(\n::Name -> s"""strcmp(${n.name}->form, "${expr.tensorValue.proceduralName}") != 0""", tensors))} ) {    
      ${out}->bufferCnt = 0;
      ${out}->buffer.numChildren = 0;
      ${out}->buffer.children = 0;
      
      unsigned long* index = GC_malloc(sizeof(unsigned long) * ${toString(fmt.dimens)});
      ${build_body(expr, order, loc)}
      {
        ${build_pack(out, fmt)}
      }
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
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      order,
      acc
    );
  
  local iv::String = head(order);
  local l::Integer =
    positionOf(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      iv,
      acc
    );
  local lc::String = toString(l);
  
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
                   ${if l != -1
                     then s"index[${lc}] = ${iv};"
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
                        isNullCond(head(points).conds) || isAllCond(head(points).conds)
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
                                  then s""
                                  else s"else if(${implode("&&", map(equals_iv(_, iv), sd))}) {"
                                }
                                
                                ${build_body(pnt.exprs, tail(order), loc)}
                                
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
                       )
                   }
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
    findSubs(expr.tensorValue, iv, tail(order));

  local subExpr::TensorAssignExpr =
    makeSub(expr, nSubs ++ subs);

  local nxtExpr::TensorAssignExpr =
    case tail(order) of
    | [] -> expr
    | _ -> makeSub(expr, nSubs ++ subs)
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
            ${emitAvailExprs(expr, iv, tail(order), env)}
            
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
                    ${code_gen(pnt.exprs, tail(order), loc, nSubs ++ subs, env)}

                    // section 6.2, emit-reduction-compute
                    ${emitReduceCompute(nxtExpr, iv, tail(order), env)}
                    // section 6.2, emit-compute
                    ${emitCompute(nxtExpr, iv, tail(order), subs, env)}
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
                     
                          ${code_gen(pnt.exprs, tail(order), loc, nSubs ++ subs, env)}
                          
                          // #7, emit-reduction-compute
                          ${emitReduceCompute(nxtExpr, iv, tail(order), env)}
                          // #7, emit-compute()
                          ${emitCompute(nxtExpr, iv, tail(order), subs, env)}
                          
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
  
  local acc::[String] =
    case ex.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
  local later::[String] =
    findDeeperSubs(ex.tensorValue, left);
  
  local super::Boolean =
    containsAny(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      left,
      acc
    );
  
  local even::Boolean =
    !containsAny(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      left,
      acc
    )
    &&
    containsBy(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      iv,
      acc
    );
  
  return
    if super
    then
      implode("\n",
        map(
          \ p::Pair<TensorExpr String> ->
          s"double ${p.snd} = ${evalExpr(p.fst, env)};"
          ,
          subs
      ))
    else if even
    then
      implode("\n",
        map(
          \ s::String ->
          s"double ${s} = 0.0;"
          ,
          later
      ))
    else "";
}

function emitReduceCompute
String ::= ex::TensorAssignExpr iv::String left::[String] env::Decorated Env
{
  local subs::[Pair<TensorExpr String>] =
    findSubs(ex.tensorValue, iv, left);
  
  local acc::[String] =
    case ex.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
  local sub::Boolean =
    !containsAny(
       \ s1::String
         s2::String
       -> s1 == s2
       ,
       left,
       acc
     )
     &&
     !containsBy(
        \ s1::String
          s2::String
        -> s1 == s2
        ,
        iv,
        acc
     );
  
  return
    if sub
    then
      implode("\n",
        map(
          \ p::Pair<TensorExpr String> ->
          s"${p.snd} += ${evalExpr(p.fst, env)};"
          ,
          subs
      ))
    else "";
}

function emitCompute
String ::= ex::TensorAssignExpr iv::String left::[String] sbs::[Pair<TensorExpr String>] env::Decorated Env
{
  local subs::[Pair<TensorExpr String>] =
    sbs
    ++ deeperSubs(ex.tensorValue, left);

  local expr::TensorAssignExpr =
    makeSub(ex, subs);  

  local acc::[String] =
    case ex.tensorAssign of
    | access(_, a) -> a
    | _ -> []
    end;
  
  local out::String =
    case ex.tensorAssign of
    | access(n, _) -> n.name
    | _ -> "error"
    end;
  
  local layer::Boolean =
    !containsAny(
       \ s1::String
         s2::String
       -> s1 == s2
       ,
       left,
       acc
    )
    &&
    containsBy(
      \ s1::String
        s2::String
      -> s1 == s2
      ,
      iv,
      acc
    );
  
  return
    if layer
    then
      s"${out}->data[p${out}${toString(listLength(acc))}] += ${evalExpr(expr.tensorValue, env)};"
    else "";
}

function deeperSubs
[Pair<TensorExpr String>] ::= ex::TensorExpr left::[String]
{
  return
    if null(left)
    then []
    else findSubs(ex, head(left), tail(left))
      ++ deeperSubs(ex, tail(left));
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
             \ a::String
               b::String
             -> a == b
             , 
             left,
             acc
           )
           &&
           containsBy(
             \ a::String
               b::String
             -> a == b
             ,
             iv,
             acc
           )
        then pair([pair(ex, s"t${iv}${toString(c)}")], c+1)
        else pair([], c)
    | tExpr(_) -> pair([], c)
    | add(l, r) ->
        if isAvail(ex, left)
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
        if isAvail(ex, left)
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
Boolean ::= ex::TensorExpr left::[String]
{
  return
    case ex of
    | nullTensorExpr() -> true
    | access(_, acc) ->
        !containsAny(
          \ a::String
            b::String
          -> a == b
          ,
          left,
          acc
         )
    | tExpr(_) -> true
    | add(l, r) -> isAvail(l, left) && isAvail(r, left)
    | mul(l, r) -> isAvail(l, left) && isAvail(r, left)
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
    | mul(l, r) -> s"(${evalExpr(l, env)} * ${evalExpr(r, env)})"
    end;
}
