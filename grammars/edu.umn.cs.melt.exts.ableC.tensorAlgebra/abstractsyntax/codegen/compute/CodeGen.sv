grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:codegen:compute;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

function codeGen
Stmt ::= nm::Name access::[String] expr::TensorExpr env::Decorated Env loc::Location
{
  local tensors::[Name] = nm :: getTensors(expr);
  local formats::[TensorFormatItem] = getFormats(tensors, env);

  local acc::[String] =
    findOrder(access,  parseOrder(expr, env));

  return if null(acc)
         then warnStmt([err(loc, s"Cannot generate code for this tensor calculation due to cyclical access pattern")])
         else parseStmt(code_gen(assignExpr(nm, access, expr, tensors, formats, location=loc), acc, loc));
}

function code_gen
String ::= expr::TensorAssignExpr order::[String] loc::Location
{
  local lattice::MergeLattice = merge_lattice(expr, head(order), loc);
  local optimized::MergeLattice = lattice_optimize(lattice, expr.tensorFormat);
  local point::LatticePoints = head(optimized.points);

  return s"""
    printf("\n${pointToString(point)}\n");
//    ${implode("\\n",
//       map(
//         \si::Pair<String Integer> ->
//         let Dj::String = s"${si.fst}${toString(si.snd)}"
//         in let Dj1::String = s"${si.fst}${toString(si.snd - 1)}"
//         in
//         s"unsigned long p${Dj} = ${Dj}_pos[p${Dj1}];"
//         end end
//         ,
//         sparse_dimensions(optimized, expr.tensorFormat)
//       )
//     )}
//    ${implode("\\n",
//       map(
//         ???, // beginning at 2
//         optimized.points
//       )
//     )}
  """;
}

function pointToString
String ::= p::LatticePoints
{
  return s"""Point with Expr ${exprToString(p.exprs)} when ${condToString(p.conds)}\n\t${implode("\\n\\t", map(pointToString(_), p.points))}""";
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
