grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:expr;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute errors::[Message];
inherited attribute parenExpr::[TensorExpr];
nonterminal TensorExpr with pp, errors, env, location, parenExpr, proceduralName;

abstract production nullTensorExpr
top::TensorExpr ::=
{
  top.pp = ppConcat([text("null")]);
  top.errors = [];
  top.proceduralName = "";
}

abstract production access
top::TensorExpr ::= name::Name access::[String]
{
  top.pp = ppConcat([
      text(name.name),
      text("("),
      ppImplode(
        text(", "),
        map(text(_), access)
      ),
      text(")")
    ]);

  top.errors =
    case lookupValue(name.name, top.env) of
    | b::[] -> case b.typerep of
               | pointerType(_,
                   tensorType(_, fmt, _)
                 ) -> if !null(fmt.tensorFormatLookupCheck)
                      then fmt.tensorFormatLookupCheck
                      else
                      let f::Decorated TensorFormatItem =
                        fmt.tensorFormatItem
                      in
                      if listLength(access) != f.dimens
                      then [err(top.location, s"Tensor ${name.name} has ${toString(f.dimens)} dimensions, but accessed using ${toString(listLength(access))} index variables.")]
                      else []
                      end
               | _ -> [err(top.location, s"Tensor access expected a tensor (got ${showType(b.typerep)}")]
               end
    | _ -> [err(top.location, s"Tensor access expcted a tensor")]
    end;
  
  top.proceduralName = s"${name.name}(${implode(",", access)})";
}

abstract production tExpr
top::TensorExpr ::= expr::Expr
{
  top.pp = expr.pp;
  expr.returnType = nothing();
  
  top.errors =
    case expr of
    | errorExpr(errs) -> errs
    | _ -> if expr.typerep.isArithmeticType
           then []
           else [err(top.location, s"Expected numeric expression (got ${showType(expr.typerep)}")]
    end;
  
  top.proceduralName = s"(${show(1000, top.pp)})";
}

abstract production add
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" + "),
    right.pp,
    text(")")
  ]);
  
  top.errors = left.errors ++ right.errors;
  
  left.parenExpr = [top];
  right.parenExpr = [top];
  
  top.proceduralName = 
    let ls::String = left.proceduralName
    in let rs::String = right.proceduralName
    in let ps::Pair<String String> =
       if ls <= rs
       then pair(ls, rs)
       else pair(rs, ls)
    in
    if null(top.parenExpr)
    then s"${ps.fst}+${ps.snd}"
    else case head(top.parenExpr) of
         | mul(_, _) -> s"(${ps.fst}+${ps.snd})"
         | div(_, _) -> s"(${ps.fst}+${ps.snd})"
         | _ -> s"${ps.fst}+${ps.snd}"
         end
    end
    end
    end;
}

abstract production sub
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" - "),
    right.pp,
    text(")")
  ]);
  
  top.errors = left.errors ++ right.errors;
  
  left.parenExpr = [top];
  right.parenExpr = [top];
  
  top.proceduralName =
    let ls::String = left.proceduralName
    in let rs::String = right.proceduralName
    in let ps::Pair<String String> =
      if ls <= rs
      then pair(ls, rs)
      else pair(rs, ls)
    in
    if null(top.parenExpr)
    then s"${ps.fst}-${ps.snd}"
    else case head(top.parenExpr) of
         | mul(_, _) -> s"(${ps.fst}-${ps.snd})"
         | div(_, _) -> s"(${ps.fst}-${ps.snd})"
         | _ -> s"${ps.fst}-${ps.snd}"
         end
    end
    end
    end;
}
abstract production mul
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" * "),
    right.pp,
    text(")")
  ]);

  top.errors = left.errors ++ right.errors;
  
  left.parenExpr = [top];
  right.parenExpr = [top];
  
  top.proceduralName =
    let ls::String = left.proceduralName
    in let rs::String = right.proceduralName
    in let ps::Pair<String String> =
       if ls <= rs
       then pair(ls, rs)
       else pair(rs, ls)
    in
    s"${ps.fst}*${ps.snd}"
    end
    end
    end;
}

abstract production div
top::TensorExpr ::= left::TensorExpr right::TensorExpr
{
  top.pp = ppConcat([
    text("("),
    left.pp,
    text(" / "),
    right.pp,
    text(")")
  ]);

  top.errors = left.errors ++ right.errors;
  
  left.parenExpr = [top];
  right.parenExpr = [top];
  
  top.proceduralName =
    let ls::String = left.proceduralName
    in let rs::String = right.proceduralName
    in let ps::Pair<String String> =
       if ls <= rs
       then pair(ls, rs)
       else pair(rs, ls)
    in
    s"${ps.fst}/${ps.snd}"
    end
    end
    end;
}

function tensorExprEqual
Boolean ::= a::TensorExpr b::TensorExpr
{
  return
    case a, b of
    | nullTensorExpr(), nullTensorExpr() -> true
    | access(n, ac), access(nm, acc) ->
        n.name == nm.name
        && foldl(
           \ b1::Boolean
             b2::Boolean
           -> b1 && b2
           ,
           true,
           zipWith(
             stringEq(_, _),
             ac,
             acc
           )
         )
    | tExpr(e1), tExpr(e2) ->
        show(1000, e1.pp) == show(1000, e2.pp)
    | add(l1, r1), add(l2, r2) ->
        tensorExprEqual(l1, l2)
        && tensorExprEqual(r1, r2)
    | sub(l1, r1), sub(l2, r2) ->
        tensorExprEqual(l1, l2)
        && tensorExprEqual(r1, r2)
    | mul(l1, r1), mul(l2, r2) ->
        tensorExprEqual(l1, l2)
        && tensorExprEqual(r1, r2)
    | div(l1, r1), div(l2, r2) ->
        tensorExprEqual(l1, l2)
        && tensorExprEqual(r1, r2)
    | _, _ -> false
    end;
}

function tensorDuplicates
[Pair<Name String>] ::= e::TensorExpr
{
  local res::[Name] = getTensors(e);

  return findDuplicates(res);
}

function replaceDuplicates
TensorExpr ::= e::TensorExpr
{
  local res::[Pair<Name String>] =
    tensorDuplicates(e);
  local nms::[Name] =
    map(
      \ p::Pair<Name String>
      -> p.fst
      ,
      nubBy(
         \ p1::Pair<Name String>
           p2::Pair<Name String>
         -> p1.fst.name == p2.fst.name
         ,
         res
      )
    );
  local pr::[Pair<Name Integer>] =
    map(pair(_, 0), nms);
  
  return replaceDuplicates_helper(e, pr).fst;
}

function replaceDuplicates_helper
Pair<TensorExpr [Pair<Name Integer>]> ::= e::TensorExpr sbs::[Pair<Name Integer>]
{
  return
    case e of
    | nullTensorExpr() -> pair(e, sbs)
    | access(t, acc) -> 
        let elem::Maybe<Pair<Name Integer>> =
          getBy(
            \ p::Pair<Name Integer>
            -> p.fst.name == t.name
            ,
            sbs
          )
        in
        case elem of
        | nothing() -> pair(e, sbs)
        | just(p) ->
            pair(
              access(
                name(t.name ++ "_" ++ toString(p.snd) ++ "_", location=t.location),
                acc,
                location=e.location
              ),
              incCnt(sbs, t)
            )
        end
        end
    | tExpr(_) -> pair(e, sbs)
    | add(l, r) ->
        let lres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(l, sbs)
        in
        let rres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(r, lres.snd)
        in
        pair(add(lres.fst, rres.fst, location=e.location), rres.snd)
        end
        end
    | sub(l, r) ->
        let lres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(l, sbs)
        in
        let rres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(r, lres.snd)
        in
        pair(sub(lres.fst, rres.fst, location=e.location), rres.snd)
        end
        end
    | mul(l, r) ->
        let lres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(l, sbs)
        in
        let rres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(r, lres.snd)
        in
        pair(mul(lres.fst, rres.fst, location=e.location), rres.snd)
        end
        end
    | div(l, r) ->
        let lres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(l, sbs)
        in
        let rres::Pair<TensorExpr [Pair<Name Integer>]>
          = replaceDuplicates_helper(r, lres.snd)
        in
        pair(div(lres.fst, rres.fst, location=e.location), rres.snd)
        end
        end
    end;
}

function incCnt
[Pair<Name Integer>] ::= lst::[Pair<Name Integer>] nm::Name
{
  return
    case lst of
    | [] -> []
    | h::tl ->
        if h.fst.name == nm.name
        then pair(h.fst, h.snd+1) :: tl
        else h :: incCnt(tl, nm)
    end;
}

function findDuplicates
[Pair<Name String>] ::= lst::[Name]
{
  return
    if null(lst)
    then []
    else
    let count::Integer =
      countBy(
        \ n1::Name n2::Name
        -> stringEq(n1.name, n2.name)
        ,
        head(lst),
        tail(lst)
      )
    in
    (if count > 0
    then generateNameSubs(head(lst), count)
    else [])
    ++
    findDuplicates(
      removeBy(
        \ n1::Name n2::Name
        -> stringEq(n1.name, n2.name)
        ,
        head(lst),
        tail(lst)
      )
    )
    end;
}

function generateNameSubs
[Pair<Name String>] ::= nm::Name cnt::Integer
{
  return
    if cnt < 0
    then []
    else 
      pair(nm, nm.name ++ "_" ++ toString(cnt) ++ "_")
      :: generateNameSubs(nm, cnt - 1);
}
