open Sexplib.Std;
open Util;

module rec Typ: {
  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(Token.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(Token.t, t)
  and sum_map = TagMap.t(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = TagMap.binding(option(t));

  /* Hazel type annotated with a relevant source location.
     Currently used to track match branches for inconsistent
     branches errors, but could perhaps be used more broadly
     for type debugging UI. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(Token.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(Token.t, t)
  and sum_map = TagMap.t(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = TagMap.binding(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };
}
and Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: Token.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TagEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend: (t, entry) => t;
  let extend_tvar: (t, tvar_entry) => t;
  let extend_alias: (t, Token.t, Id.t, Typ.t) => t;
  let extend_dummy_tvar: (t, Token.t) => t;
  let lookup: (t, Token.t) => option(entry);
  let lookup_tvar: (t, Token.t) => option(tvar_entry);
  let lookup_alias: (t, Token.t) => option(Typ.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: Token.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TagEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend = (ctx, entry) => List.cons(entry, ctx);

  let lookup = (ctx, name) =>
    List.find_map(
      fun
      | Ctx.VarEntry(v) when v.name == name => Some(VarEntry(v))
      | TagEntry(v) when v.name == name => Some(TagEntry(v))
      | TVarEntry(v) when v.name == name => Some(TVarEntry(v))
      | _ => None,
      ctx,
    );

  let extend_tvar = (ctx: t, tvar_entry: tvar_entry): t =>
    extend(ctx, TVarEntry(tvar_entry));

  let extend_alias = (ctx: t, name: Token.t, id: Id.t, ty: Typ.t): t =>
    extend_tvar(ctx, {name, id, kind: Singleton(ty)});

  let extend_dummy_tvar = (ctx: t, name: Token.t) =>
    extend_tvar(ctx, {kind: Abstract, name, id: Id.invalid});

  let lookup_tvar = (ctx: t, name: Token.t): option(tvar_entry) =>
    switch (lookup(ctx, name)) {
    | Some(TVarEntry(t)) => Some(t)
    | _ => None
    };

  let lookup_alias = (ctx: t, t: Token.t): option(Typ.t) =>
    switch (lookup_tvar(ctx, t)) {
    | Some({kind: Singleton(ty), _}) => Some(ty)
    | Some({kind: Abstract, _})
    | _ => None
    };
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
};
