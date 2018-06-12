grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

marking terminal Build_t /build[\ \t\n\r]+tensor/  lexer classes {Ckeyword};
marking terminal Tensor_t 'tensor' lexer classes {Ckeyword};
marking terminal Format_t 'format' lexer classes {Ckeyword};
marking terminal Value_t 'value' lexer classes {Ckeyword};
marking terminal TensorFormat_t /tensor[\ \t\n\r]+format/ lexer classes {Ckeyword};


concrete productions top::AssignExpr_c
| 'format' '(' '{' specs::SpecifierList_c '}' ')'
  {
    top.ast = format_without(specs.specs, location=top.location);
  }
| 'format' '(' '{' specs::SpecifierList_c '}' ',' '{' order::OrderList_c '}' ')'
  {
    top.ast = format_with(specs.specs, order.specs, location=top.location);
  }
| b::Build_t '(' '{' dims::IndexList_c '}' ',' fmt::Expr_c ')'
  {
    top.ast = tensor_empty(dims.indxs, fmt.ast, location=top.location);
  }
| b::Build_t '(' data::TensorRep_c ',' fmt::Expr_c ')'
  {
    top.ast = tensor_filled(data.tensor, fmt.ast, location=top.location);
  }
| 'value' tensor::Expr_c '{' idx::IndexList_c '}'
  {
    top.ast = mkIntConst(0, top.location);
    --top.ast = tensor_get(fromId(tensor), idx.indxs, location=top.location);
  }


concrete productions top::Stmt_c
| 'value' tensor::Expr_c '{' idx::IndexList_c '}' '=' val::Expr_c ';'
  {
    top.ast = nullStmt();
  }

concrete productions top::TypeSpecifier_c
| t::TensorFormat_t
  {
    top.realTypeSpecifiers = [formatTypeExpr(top.givenQualifiers)];
    top.preTypeSpecifiers = [];
  }
| 'tensor'
  {
    top.realTypeSpecifiers = [tensorTypeExpr(top.givenQualifiers)];
    top.preTypeSpecifiers = [];
  }

terminal Dense_t 'dense';
terminal Sparse_t 'sparse';


synthesized attribute specs::[Integer];
nonterminal SpecifierList_c with location, specs;
concrete productions top::SpecifierList_c
| 'dense' ',' lst::SpecifierList_c
  {
    top.specs = storeDense :: lst.specs;
  }
| 'sparse' ',' lst::SpecifierList_c
  {
    top.specs = storeSparse :: lst.specs;
  }
| 'dense'
  {
    top.specs = storeDense :: [];
  }
| 'sparse'
  {
    top.specs = storeSparse :: [];
  }


nonterminal OrderList_c with location, specs;
concrete productions top::OrderList_c
| e::DecConstant_t ',' lst::OrderList_c 
  {
    top.specs = toInt(e.lexeme) :: lst.specs;
  }
| e::DecConstant_t
  {
    top.specs = toInt(e.lexeme) :: [];
  }


synthesized attribute indxs::[Expr];
nonterminal IndexList_c with location, indxs;
concrete productions top::IndexList_c
| e::AssignExpr_c ',' lst::IndexList_c 
  {
    top.indxs = e.ast :: lst.indxs;
  }
| e::AssignExpr_c
  {
    top.indxs = e.ast :: [];
  }


synthesized attribute tensor::Tensor;
nonterminal TensorRep_c with location, tensor;
concrete productions top::TensorRep_c
| '[' vs::NumericList_c ']' {
    top.tensor = tensor_base(vs.data, location=top.location);
  }
| '[' ts::TensorList_c ']' {
    top.tensor = tensor_higher(ts.tensors, location=top.location);
  }

synthesized attribute data::[Expr];
nonterminal NumericList_c with location, data;
concrete productions top::NumericList_c
| e::AssignExpr_c ',' lst::NumericList_c {
    top.data = e.ast :: lst.data;
  }
| e::AssignExpr_c {
    top.data = e.ast :: [];
  }

synthesized attribute tensors::[Tensor];
nonterminal TensorList_c with location, tensors;
concrete productions top::TensorList_c
| e::TensorRep_c ',' lst::TensorList_c {
    top.tensors = e.tensor :: lst.tensors;
  }
| e::TensorRep_c {
    top.tensors = e.tensor :: [];
}
