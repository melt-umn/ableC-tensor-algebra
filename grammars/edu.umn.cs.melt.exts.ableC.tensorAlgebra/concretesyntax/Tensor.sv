grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute tensor::TensorConstant;
tracked nonterminal TensorRep_c with tensor;
concrete productions top::TensorRep_c
| '[' values::ExprList_c ']' 
  {
    top.tensor = tensor_base(values.list);
  }
| '[' tensors::TensorRepList_c ']'
  {
    top.tensor = tensor_higher(tensors.list);
  }
