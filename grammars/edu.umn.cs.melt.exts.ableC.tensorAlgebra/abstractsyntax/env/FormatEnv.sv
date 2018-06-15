grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:env;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute specifiers::[Integer];
synthesized attribute dimenOrder::[Integer];
synthesized attribute proceduralName::String;
synthesized attribute dimens::Integer;

nonterminal TensorFormatItem with specifiers, dimenOrder, proceduralName, dimens, sourceLocation;

abstract production tensorFormatItem
top::TensorFormatItem ::= specifiers::[Integer] dimenOrder::[Integer] source::Location
{
  top.specifiers = specifiers;
  top.dimenOrder = dimenOrder;
  top.dimens = arrayLength(specifiers);
  top.proceduralName = formName(specifiers, dimenOrder);
  top.sourceLocation = source;
}

function formName
String ::= specs::[Integer] order::[Integer]
{
  return case specs, order of
         | [], [] -> ""
         | s::ss, o::os ->
           (if s == storeDense
            then "d"
            else "s")
           ++ toString(o) ++ formName(ss, os)
         | _, _ -> "error"
         end;
}

abstract production errorTensorFormatItem
top::TensorFormatItem ::=
{
  top.specifiers = [];
  top.dimenOrder = [];
  top.dimens = 0;
  top.proceduralName = "error";
  top.sourceLocation = builtin;
}


synthesized attribute tensorFormats::Scopes<TensorFormatItem> occurs on Env;
synthesized attribute tensorFormatContribs::Contribs<TensorFormatItem> occurs on Defs, Def;

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
top::Def ::= s::String fmt::TensorFormatItem
{
  top.tensorFormatContribs = [pair(s, fmt)];
}

function lookupTensorFormat
[TensorFormatItem] ::= n::String e::Decorated Env
{
  return lookupScope(n, e.tensorFormats);
}


synthesized attribute tensorFormatItem::Decorated TensorFormatItem occurs on Name;
synthesized attribute tensorFormatLookupCheck::[Message] occurs on Name;
synthesized attribute tensorFormatRedeclarationCheck::[Message] occurs on Name;

aspect production name
top::Name ::= n::String
{
  local tensorFormats::[TensorFormatItem] = lookupTensorFormat(n, top.env);
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
  
  local tensorFormat::TensorFormatItem = 
    if null(tensorFormats)
    then errorTensorFormatItem()
    else head(tensorFormats);
  
  top.tensorFormatItem = tensorFormat;
}
