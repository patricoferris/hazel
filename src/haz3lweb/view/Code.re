open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    ((label, is_in_buffer, sort, is_consistent, is_complete, i)) => {
      let cls =
        switch (label) {
        | _ when is_in_buffer => "in-buffer"
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when s == Form.explicit_hole => "explicit-hole"
        | [s] when Form.is_string(s) => "string-lit"
        | _ => "default"
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      let label = is_in_buffer ? AssistantExpander.mark(label) : label;
      [
        span(
          ~attrs=[
            Attr.classes(["token", cls, Sort.to_string(sort), plurality]),
          ],
          [Node.text(List.nth(label, i))],
        ),
      ];
    },
  );
let of_delim =
    (is_in_buffer, is_consistent, t: Piece.tile, i: int): list(Node.t) =>
  of_delim'((
    t.label,
    is_in_buffer,
    t.mold.out,
    is_consistent,
    Tile.is_complete(t),
    i,
  ));

let of_grout = [Node.text(Unicode.nbsp)];

let of_secondary =
  Core.Memo.general(
    ~cache_size_bound=10000, ((content, secondary_icons, indent)) =>
    if (String.equal(Secondary.get_string(content), Form.linebreak)) {
      let str = secondary_icons ? Form.linebreak : "";
      [
        span_c("linebreak", [text(str)]),
        Node.br(),
        Node.text(StringUtil.repeat(indent, Unicode.nbsp)),
      ];
    } else if (String.equal(Secondary.get_string(content), Form.space)) {
      let str = secondary_icons ? "·" : Unicode.nbsp;
      [span_c("secondary", [text(str)])];
    } else if (Secondary.content_is_comment(content)) {
      [span_c("comment", [Node.text(Secondary.get_string(content))])];
    } else {
      [span_c("secondary", [Node.text(Secondary.get_string(content))])];
    }
  );

module Text = (M: {
                 let map: Measured.t;
                 let settings: Settings.t;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (buffer_ids, no_sorts, sort, seg: Segment.t): list(Node.t) => {
    /* note: no_sorts flag is used for backback view;
       otherwise Segment.expected_sorts call crashes for some reason */
    let expected_sorts =
      no_sorts
        ? List.init(List.length(seg), i => (i, Sort.Any))
        : Segment.expected_sorts(sort, seg);
    let sort_of_p_idx = idx =>
      switch (List.assoc_opt(idx, expected_sorts)) {
      | None => Sort.Any
      | Some(sort) => sort
      };
    seg
    |> List.mapi((i, p) => (i, p))
    |> List.concat_map(((i, p)) =>
         of_piece(buffer_ids, sort_of_p_idx(i), p)
       );
  }
  and of_piece =
      (buffer_ids, expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(buffer_ids, expected_sort, t)
    | Grout(_) => of_grout
    | Secondary({content, _}) =>
      of_secondary((content, M.settings.secondary_icons, m(p).last.col))
    };
  }
  and of_tile = (buffer_ids, expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          //TODO(andrew): more subtle logic about sort acceptability
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    let is_in_buffer = List.mem(t.id, buffer_ids);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(of_delim(is_in_buffer, is_consistent, t), ((seg, sort)) =>
         of_segment(buffer_ids, false, sort, seg)
       )
    |> List.concat;
  };
};

let rec holes =
        (~font_metrics, ~map: Measured.t, seg: Segment.t): list(Node.t) =>
  seg
  |> List.concat_map(
       fun
       | Piece.Secondary(_) => []
       | Tile(t) => List.concat_map(holes(~map, ~font_metrics), t.children)
       | Grout(g) => [
           EmptyHoleDec.view(
             ~font_metrics, // TODO(d) fix sort
             {
               measurement: Measured.find_g(g, map),
               mold: Mold.of_grout(g, Any),
             },
           ),
         ],
     );

let simple_view = (~unselected, ~map, ~settings: Settings.t): Node.t => {
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  div(
    ~attrs=[Attr.class_("code")],
    [span_c("code-text", Text.of_segment([], false, Sort.Any, unselected))],
  );
};

let view =
    (
      ~buffer_ids: list(Uuidm.t),
      ~sort: Sort.t,
      ~font_metrics,
      ~segment,
      ~unselected,
      ~measured,
      ~settings: Settings.t,
    )
    : Node.t => {
  module Text =
    Text({
      let map = measured;
      let settings = settings;
    });
  let unselected = Text.of_segment(buffer_ids, false, sort, unselected);
  let holes = holes(~map=measured, ~font_metrics, segment);
  div(
    ~attrs=[Attr.class_("code")],
    [span_c("code-text", unselected), ...holes],
  );
};
