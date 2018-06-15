grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:syntax;

synthesized attribute rep::String;
synthesized attribute fwrd::((Stmt ::= Expr [Expr] Expr) ::= String);
nonterminal AssignmentOp with rep, fwrd;

abstract production eqAssign
top::AssignmentOp ::=
{
  top.rep = "=";
  top.fwrd = \fmtNm::String -> (\t::Expr lst::[Expr] v::Expr ->
    substStmt(
      declRefSubstitution(s"__value", v) ::
        declRefSubstitution(s"__tensor", t) ::
          generateSubstitutions(lst, 0),
      parseStmt(s"""
      {
        struct tensor_${fmtNm}* _tensor = __tensor;
        double _value = __value;
        unsigned long __index[] = {${generateArray(lst, 0)}};
        tensor_modify_${fmtNm}(_tensor, __index, _value);
      }
      """)
    ));
}

abstract production addAssign
top::AssignmentOp ::=
{
  top.rep = "+=";
  top.fwrd = \fmtNm::String -> (\t::Expr lst::[Expr] v::Expr ->
    substStmt(
      declRefSubstitution(s"__value", v) ::
        declRefSubstitution(s"__tensor", t) ::
          generateSubstitutions(lst, 0),
      parseStmt(s"""
      {
        struct tensor_${fmtNm}* _tensor = __tensor;
        double _value = __value;
        unsigned long __index[] = {${generateArray(lst, 0)}};
        _tensor->pack(_tensor);
        double _val = tensor_get_${fmtNm}(_tensor, __index);
        tensor_modify_${fmtNm}(_tensor, __index, _val + _value);
      }
      """)
    ));
}

abstract production subAssign
top::AssignmentOp ::=
{
  top.rep = "-=";
  top.fwrd = \fmtNm::String -> (\t::Expr lst::[Expr] v::Expr ->
    substStmt(
      declRefSubstitution(s"__value", v) ::
        declRefSubstitution(s"__tensor", t) ::
          generateSubstitutions(lst, 0),
      parseStmt(s"""
      {
        struct tensor_${fmtNm}* _tensor = __tensor;
        double _value = __value;
        unsigned long __index[] = {${generateArray(lst, 0)}};
        _tensor->pack(_tensor);
        double _val = tensor_get_${fmtNm}(_tensor, __index);
        tensor_modify_${fmtNm}(_tensor, __index, _val - _value);
      }
      """)
    ));
}

abstract production mulAssign
top::AssignmentOp ::=
{
  top.rep = "*=";
  top.fwrd = \fmtNm::String -> (\t::Expr lst::[Expr] v::Expr ->
    substStmt(
      declRefSubstitution(s"__value", v) ::
        declRefSubstitution(s"__tensor", t) ::
          generateSubstitutions(lst, 0),
      parseStmt(s"""
      {
        struct tensor_s* _tensor = __tensor;
        double _value = __value;
        unsigned long __index[] = {${generateArray(lst, 0)}};
        _tensor->pack(_tensor);
        double _val = tensor_get_${fmtNm}(_tensor, __index);
        tensor_modify_${fmtNm}(_tensor, __index, _val * _value);
      }
      """)
    ));
}

abstract production divAssign
top::AssignmentOp ::= 
{
  top.rep = "/=";
  top.fwrd = \fmtNm::String -> (\t::Expr lst::[Expr] v::Expr ->
    substStmt(
      declRefSubstitution(s"__value", v) ::
        declRefSubstitution(s"__tensor", t) ::
          generateSubstitutions(lst, 0),
      parseStmt(s"""
      {
        struct tensor_s* _tensor = __tensor;
        double _value = __value;
        unsigned long __index[] = {${generateArray(lst, 0)}};
        _tensor->pack(_tensor);
        double _val = tensor_get_${fmtNm}(_tensor, __index);
        tensor_modify_${fmtNm}(_tensor, __index, _val / _value);
      }
      """)
    ));
}
