grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra;

exports edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax;
exports edu:umn:cs:melt:exts:ableC:tensorAlgebra:concretesyntax;
exports edu:umn:cs:melt:exts:ableC:tensorAlgebra:utils;

exports edu:umn:cs:melt:exts:ableC:templating;
exports edu:umn:cs:melt:exts:ableC:halide;

global storeSparse::Integer = 0;
global storeDense ::Integer = 1;

global emitParallel::String =
  "edu_umn_cs_melt_exts_ableC_tensorAlgebra_parallel";
global emitThreads::String =
  "edu_umn_cs_melt_exts_ableC_tensorAlgebra_threads";
