grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute list<a>::[a];

nonterminal SpecifierList_c with list<Integer>, location;
concrete productions top::SpecifierList_c
| 'dense'
  {
    top.list = [storeDense];
  }
| 'sparse'
  {
    top.list = [storeSparse];
  }
| 'dense' ',' tl::SpecifierList_c
  {
    top.list = storeDense :: tl.list;
  }
| 'sparse' ',' tl::SpecifierList_c
  {
    top.list = storeSparse :: tl.list;
  }


nonterminal IntegerList_c with list<Integer>, location;
concrete productions top::IntegerList_c
| num::DecConstant_t
  {
    top.list = [toInteger(num.lexeme)];
  }
| num::DecConstant_t ',' lst::IntegerList_c
  {
    top.list = toInteger(num.lexeme) :: lst.list;
  }

nonterminal NameList_c with list<Name>, location;
concrete productions top::NameList_c
| nm::Identifier_t
  {
    top.list = [fromId(nm)];
  }
| nm::Identifier_t ',' lst::NameList_c
  {
    top.list = fromId(nm) :: lst.list;
  }


nonterminal TensorRepList_c with list<TensorConstant>, location;
concrete productions top::TensorRepList_c
| rep::TensorRep_c
  {
    top.list = [rep.tensor];
  }
| rep::TensorRep_c ',' lst::TensorRepList_c
  {
    top.list = rep.tensor :: lst.list;
  }


nonterminal ExprList_c with list<Expr>, location;
concrete productions top::ExprList_c
| ex::AssignExpr_c
  {
    top.list = ex.ast :: [];
  }
| ex::AssignExpr_c ',' lst::ExprList_c
  {
    top.list = ex.ast :: lst.list;
  }
