grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;

marking terminal Build_t 'build' lexer classes {Ckeyword};
marking terminal Tensor_t 'tensor' lexer classes {Ckeyword};
marking terminal Indexvar_t 'indexvar' lexer classes {Ckeyword};

marking terminal TensorTransform_t /tensor[\ \t\r\n\f]+transform/ lexer classes {Ckeyword};

marking terminal Orderof_t 'orderof' lexer classes {Ckeyword};
marking terminal Dimensof_t 'dimenof' lexer classes {Ckeyword};

terminal Dense_t 'dense';
terminal Sparse_t 'sparse';
terminal Format_t 'format';

