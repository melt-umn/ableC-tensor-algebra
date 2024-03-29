grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:format;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

{- Adding tensor formats to the ableC Env -}

synthesized attribute proceduralName::String;
synthesized attribute dimensions::Integer;
synthesized attribute storage::[(Integer, Integer, Integer)]; -- (order, dimen, spec)

tracked nonterminal TensorFormat with proceduralName, dimensions, storage;

abstract production tensorFormat
top::TensorFormat ::= specifiers::[Integer] dimenOrder::[Integer]
{
  top.proceduralName = 
    case specifiers, dimenOrder of
    | [], [] -> "__error"
    | _, _ -> formName(specifiers, dimenOrder)
    end;
  top.dimensions = listLength(specifiers);
  top.storage = formStorage(specifiers, dimenOrder, 0);
}

abstract production errorTensorFormat
top::TensorFormat ::=
{
  top.proceduralName = "error";
  top.dimensions = -1;
  top.storage = [];
}


synthesized attribute tensorFormats::Scopes<TensorFormat> occurs on Env;
synthesized attribute tensorFormatContribs::Contribs<TensorFormat> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
  top.tensorFormats = [tm:empty()];
}

aspect production addEnv_i
top::Env ::= d::Defs e::Decorated Env
{
  top.tensorFormats = addGlobalScope(gd.tensorFormatContribs, addScope(d.tensorFormatContribs, e.tensorFormats));
}

aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.tensorFormats = tm:empty() :: e.tensorFormats;
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

aspect production functionEnv_i
top::Env ::= e::Decorated Env
{
  top.tensorFormats = functionScope(e.tensorFormats);
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
  top.tensorFormatContribs = [(s, fmt)];
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
    | [] -> [errFromOrigin(top, "Undeclared tensor format " ++ n)]
    | _ :: _ -> []
    end;
  
  top.tensorFormatRedeclarationCheck =
    case tensorFormats of
    | [] -> []
    | v :: _ ->
      [errFromOrigin(top,
        "Redeclaration of " ++ n ++ ". Original (from " ++
        getParsedOriginLocationOrFallback(v).unparse ++ ")")]
    end;
  
  local tensorFormat::TensorFormat =
    if null(tensorFormats)
    then errorTensorFormat()
    else head(tensorFormats);
  
  top.tensorFormat = tensorFormat;
}
