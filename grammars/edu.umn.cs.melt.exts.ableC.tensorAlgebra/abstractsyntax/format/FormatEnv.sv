grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute proceduralName::String;
synthesized attribute dimensions::Integer;
synthesized attribute storage::[Pair<Integer Pair<Integer Integer>>]; -- pair(order, pair(dimen, spec))

nonterminal TensorFormat with proceduralName, dimensions, storage, sourceLocation;

abstract production tensorFormat
top::TensorFormat ::= specifiers::[Integer] dimenOrder::[Integer] source::Location
{
  top.proceduralName = 
    case specifiers, dimenOrder of
    | [], [] -> "scalar"
    | _, _ -> formName(specifiers, dimenOrder)
    end;
  top.dimensions = listLength(specifiers);
  top.storage = formStorage(specifiers, dimenOrder, 0);
  top.sourceLocation = source;
}

abstract production errorTensorFormat
top::TensorFormat ::=
{
  top.proceduralName = "error";
  top.dimensions = -1;
  top.storage = [];
  top.sourceLocation = builtin;
}


synthesized attribute tensorFormats::Scopes<TensorFormat> occurs on Env;
synthesized attribute tensorFormatContribs::Contribs<TensorFormat> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
  top.tensorFormats = [tm:empty(compareString)];
}

aspect production addEnv_i
top::Env ::= d::Defs e::Decorated Env
{
  top.tensorFormats = addGlobalScope(gd.tensorFormatContribs, addScope(d.tensorFormatContribs, e.tensorFormats));
}

aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.tensorFormats = tm:empty(compareString) :: e.tensorFormats;
}

aspect production globalEnv_i
top::Env ::= e::Decorated Env
{
  top.tensorFormats = [last(e.tensorFormats)];
}

aspect production nonGlobalEnv_i
top::Env ::= e::Decorated Env
{
  top.tensorFormats = nonGlobalScope(e.tensorFormats);
}

aspect production nilDefs
top::Defs ::=
{
  top.tensorFormatContribs = [];
}

aspect production consDefs
top::Defs ::= h::Def t::Defs
{
  top.tensorFormatContribs = h.tensorFormatContribs ++ t.tensorFormatContribs;
}

aspect default production
top::Def ::=
{
  top.tensorFormatContribs = [];
}


abstract production tensorFormatDef
top::Def ::= s::String fmt::TensorFormat
{
  top.tensorFormatContribs = [pair(s, fmt)];
}

function lookupTensorFormat
[TensorFormat] ::= n::String e::Decorated Env
{
  return lookupScope(n, e.tensorFormats);
}


synthesized attribute tensorFormat::Decorated TensorFormat occurs on Name;
synthesized attribute tensorFormatLookupCheck::[Message] occurs on Name;
synthesized attribute tensorFormatRedeclarationCheck::[Message] occurs on Name;

aspect production name
top::Name ::= n::String
{
  local tensorFormats::[TensorFormat] = lookupTensorFormat(n, top.env);
  top.tensorFormatLookupCheck =
    case tensorFormats of
    | [] -> [err(top.location, "Undeclared tensor format " ++ n)]
    | _ :: _ -> []
    end;
  
  top.tensorFormatRedeclarationCheck =
    case tensorFormats of
    | [] -> []
    | v :: _ ->
      [err(top.location,
        "Redeclaration of " ++ n ++ ". Original (from line " ++
        toString(v.sourceLocation.line) ++ ")")]
    end;
  
  local tensorFormat::TensorFormat =
    if null(tensorFormats)
    then errorTensorFormat()
    else head(tensorFormats);
  
  top.tensorFormat = tensorFormat;
}
