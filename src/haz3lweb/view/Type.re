open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attrs=[clss(["typ-view", cls])], [text(s)]);

let alias_view = (s: string): Node.t =>
  div(~attrs=[clss(["typ-alias-view"])], [text(s)]);

let prov_view: Typ.type_provenance => Node.t =
  fun
  | Internal => div([])
  | Free(name) =>
    div(~attrs=[clss(["typ-mod", "free-type-var"])], [text(name)])
  | TypeHole =>
    div(~attrs=[clss(["typ-mod", "type-hole"])], [text("𝜏")])
  | SynSwitch =>
    div(~attrs=[clss(["typ-mod", "syn-switch"])], [text("⇒")]);

let rec view_ty = (ty: Haz3lcore.Typ.t): Node.t =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(prov) =>
    div(
      ~attrs=[
        clss(["typ-view", "atom", "unknown"]),
        Attr.title(Typ.show_type_provenance(prov)),
      ],
      [text("?") /*, prov_view(prov)*/],
    )
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var(name) => ty_view("Var", name)
  | Rec(x, t) =>
    div(
      ~attrs=[clss(["typ-view", "Rec"])],
      [text("Rec " ++ x ++ ". "), view_ty(t)],
    )
  | List(t) =>
    div(
      ~attrs=[clss(["typ-view", "atom", "List"])],
      [text("["), view_ty(t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attrs=[clss(["typ-view", "Arrow"])],
      [view_ty(t1), text(" -> "), view_ty(t2)],
    )
  | Prod([]) => div(~attrs=[clss(["typ-view", "Prod"])], [text("()")])
  | Prod([_]) =>
    div(~attrs=[clss(["typ-view", "Prod"])], [text("Singleton Product")])
  | Prod([t0, ...ts]) =>
    div(
      ~attrs=[clss(["typ-view", "atom", "Prod"])],
      [
        text("("),
        div(
          ~attrs=[clss(["typ-view", "Prod"])],
          [view_ty(t0)]
          @ (List.map(t => [text(", "), view_ty(t)], ts) |> List.flatten),
        ),
        text(")"),
      ],
    )
  | Sum(ts) =>
    div(
      ~attrs=[clss(["typ-view", "Sum"])],
      switch (ts) {
      | [] => [text("Nullary Sum")]
      | [t0] => [text("+")] @ ctr_view(t0)
      | [t0, ...ts] =>
        let ts_views =
          List.map(t => [text(" + ")] @ ctr_view(t), ts) |> List.flatten;
        ctr_view(t0) @ ts_views;
      },
    )
  }
and ctr_view = ((ctr, typ)) =>
  switch (typ) {
  | None => [text(ctr)]
  | Some(typ) => [text(ctr ++ "("), view_ty(typ), text(")")]
  };

let view = (ty: Haz3lcore.Typ.t): Node.t =>
  div_c("typ-wrapper", [view_ty(ty)]);
