grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;


concrete productions top::Declaration_c
| 'tensor' 'format' nm::Identifier_t '(' '{' specs::SpecifierList_c '}' ')' ';'
  {
    top.ast = format(fromId(nm), specs.list, makeList(inc, 0, listLength(specs.list)));
  }
| 'tensor' 'format' nm::Identifier_t '(' '{' specs::SpecifierList_c '}' ',' '{' order::IntegerList_c '}' ')' ';'
  {
    top.ast = format(fromId(nm), specs.list, order.list);
  }
| 'indexvar' vars::NameList_c ';'
  {
    top.ast = indexvar(vars.list);
  }

concrete productions top::AssignExpr_c
| 'build' '(' type::TypeName_c ')' '(' '{' dims::ExprList_c  '}' ')' 
  {
    top.ast = build_empty(type.ast, dims.list);
  }
| 'build' '(' type::TypeName_c ')' '(' data::TensorRep_c ')'
  {
    top.ast = build_data(type.ast, data.tensor);
  }
| 'build' '(' type::TypeName_c ')' '(' args::ArgumentExprList_c ')'
  {
    top.ast = build(type.ast, args.ast);
  }

concrete productions top::UnaryExpr_c
| 'orderof' '(' tp::TypeName_c ')'
  {
    top.ast = orderofType(tp.ast);
  }
| 'orderof' '(' tn::Expr_c ')'
  {
    top.ast = orderofExpr(tn.ast);
  }
| 'dimenof' '(' tn::Expr_c ')' '[' dim::Expr_c ']'
  {
    top.ast = dimenof(tn.ast, dim.ast);
  }
| 'freeTensor' '(' tn::Expr_c ')'
  {
    top.ast = freeTensor(tn.ast);
  }

concrete productions top::TypeSpecifier_c
| 'tensor' '<' fmt::Identifier_t '>'
  {
    top.realTypeSpecifiers = [tensorTypeExpr(top.givenQualifiers, fromId(fmt))];
    top.preTypeSpecifiers = [];
  }
