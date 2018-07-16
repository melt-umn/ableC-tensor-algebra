grammar edu:umn:cs:melt:exts:ableC:tensorAlgebra:abstractsyntax:indexvar;

import edu:umn:cs:melt:exts:ableC:tensorAlgebra;

synthesized attribute variable::String;

nonterminal IndexVar with variable, sourceLocation;

abstract production indexVar
top::IndexVar ::= nm::Name source::Location
{
  top.variable = nm.name;
  top.sourceLocation = source;
}

abstract production errorIndexVar
top::IndexVar ::=
{
  top.variable = "";
  top.sourceLocation = builtin;
}


synthesized attribute indexVars::Scopes<IndexVar> occurs on Env;
synthesized attribute indexVarContribs::Contribs<IndexVar> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
  top.indexVars = [tm:empty(compareString)];
}

aspect production addEnv_i
top::Env ::= d::Defs e::Decorated Env
{
  top.indexVars = addGlobalScope(gd.indexVarContribs, addScope(d.indexVarContribs, e.indexVars));
}

aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.indexVars = tm:empty(compareString) :: e.indexVars;
}

aspect production globalEnv_i
top::Env ::= e::Decorated Env
{
  top.indexVars = [last(e.indexVars)];
}

aspect production nonGlobalEnv_i
top::Env ::= e::Decorated Env
{
  top.indexVars = nonGlobalScope(e.indexVars);
}

aspect production nilDefs
top::Defs ::=
{
  top.indexVarContribs = [];
}

aspect production consDefs
top::Defs ::= h::Def t::Defs
{
  top.indexVarContribs = h.indexVarContribs ++ t.indexVarContribs;
}

aspect default production
top::Def ::=
{
  top.indexVarContribs = [];
}


abstract production indexVarDef
top::Def ::= s::String var::IndexVar
{
  top.indexVarContribs = [pair(s, var)];
}

function lookupIndexVar
[IndexVar] ::= n::String e::Decorated Env
{
  return lookupScope(n, e.indexVars);
}


synthesized attribute indexVar::Decorated IndexVar occurs on Name;
synthesized attribute indexVarLookupCheck::[Message] occurs on Name;
synthesized attribute indexVarRedeclarationCheck::[Message] occurs on Name;

aspect production name
top::Name ::= n::String
{
  local indexVars::[IndexVar] = lookupIndexVar(n, top.env);
  top.indexVarLookupCheck =
    case indexVars of
    | [] -> [err(top.location, "Undeclared index variable " ++ n)]
    | _ :: _ -> []
    end;
  
  top.indexVarRedeclarationCheck =
    case indexVars of
    | [] -> []
    | v :: _ ->
      [err(top.location,
        "Redeclaration of " ++ n ++ ". Original (from line " ++
        toString(v.sourceLocation.line) ++ ")")]
    end;

  local indexVar::IndexVar =
    if null(indexVars)
    then errorIndexVar()
    else head(indexVars);
  
  top.indexVar = indexVar;
}
