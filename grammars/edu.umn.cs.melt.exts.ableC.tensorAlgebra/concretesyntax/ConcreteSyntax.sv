grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

marking terminal Build_t /build/  lexer classes {Ckeyword};
marking terminal Tensor_t 'tensor' lexer classes {Ckeyword};
marking terminal Format_t 'format' lexer classes {Ckeyword};
marking terminal Value_t 'value' lexer classes {Ckeyword};
marking terminal TensorFormat_t /tensor[\ \t\n\r]+format/ lexer classes {Ckeyword};

concrete productions top::ExternalDeclaration_c
| t::TensorFormat_t nm::Identifier_t '=' '(' '{' specs::SpecifierList_c '}' ')' ';'
  {
    top.ast = format_without(fromId(nm), specs.specs);
  }
| t::TensorFormat_t nm::Identifier_t '=' '(' '{' specs::SpecifierList_c '}' ',' '{' order::OrderList_c '}' ')' ';'
  {
    top.ast = format_with(fromId(nm), specs.specs, order.specs);
  }

concrete productions top::AssignExpr_c
| b::Build_t '(' type::TypeName_c ')' '(' '{' dims::IndexList_c '}' ')'
  {
    top.ast = tensor_empty(type.ast, dims.indxs, location=top.location);
  }
| b::Build_t '(' type::TypeName_c ')' '(' data::TensorRep_c ')'
  {
    top.ast = tensor_filled(type.ast, data.tensor, location=top.location);
  }
| b::Build_t '(' type::TypeName_c ')' '(' dims::Expr_c ')'
  {
    top.ast = tensor_array(type.ast, dims.ast, location=top.location);
  }
| 'value' '(' tensor::Expr_c ')' '(' idx::IndexList_c ')'
  {
    top.ast = tensor_get(tensor.ast, idx.indxs, location=top.location);
  }


concrete productions top::Stmt_c
| 'value' '(' tensor::Expr_c ')' '(' idx::IndexList_c ')' op::AssignmentOp_c val::Expr_c ';'
  {
    top.ast = tensor_assign(tensor.ast, idx.indxs, op.assignOp, val.ast);
  }

concrete productions top::TypeSpecifier_c
| 'tensor' '<' fmt::Identifier_t '>'
  {
    top.realTypeSpecifiers = [tensorTypeExpr(top.givenQualifiers, fromId(fmt))];
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


synthesized attribute assignOp::AssignmentOp;
nonterminal AssignmentOp_c with assignOp;
concrete productions top::AssignmentOp_c
| '=' {
    top.assignOp = eqAssign();
  }
| '+=' {
    top.assignOp = addAssign();
  }
| '-=' {
    top.assignOp = subAssign();
  }
| '*=' {
    top.assignOp = mulAssign();
  }
| '/=' {
    top.assignOp = divAssign();
  }
