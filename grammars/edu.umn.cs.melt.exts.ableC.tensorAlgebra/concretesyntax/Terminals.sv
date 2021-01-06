grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

marking terminal Build_t 'build' lexer classes {Keyword, Global};
marking terminal Tensor_t 'tensor' lexer classes {Keyword, Global};
marking terminal Indexvar_t 'indexvar' lexer classes {Keyword, Global};

marking terminal TensorTransform_t /tensor[\ \t\r\n]+transform/ lexer classes {Keyword, Global};

marking terminal ForEach_t 'foreach' lexer classes {Keyword, Global};
terminal In_t 'in' lexer classes {Keyword};

marking terminal Orderof_t 'orderof' lexer classes {Keyword, Global};
marking terminal Dimensof_t 'dimenof' lexer classes {Keyword, Global};
marking terminal FreeTensor_t 'freeTensor' lexer classes {Keyword, Global};

terminal Dense_t 'dense' lexer classes {Keyword};
terminal Sparse_t 'sparse' lexer classes {Keyword};
terminal Format_t 'format' lexer classes {Keyword};

terminal Order_t 'order' lexer classes {Keyword};
terminal Loops_t 'loops' lexer classes {Keyword};
