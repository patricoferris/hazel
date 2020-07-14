module Vdom = Virtual_dom.Vdom;

type rects = list(RectilinearPolygon.rect);

type current_term_rects = {
  highlighted: rects,
  closed_children: list((TermSort.t, rects)),
};

let rects =
    (~vtrim=0.0, ~indent=0, start: CaretPosition.t, m: MeasuredLayout.t)
    : (CaretPosition.t, rects) => {
  let mk_rect =
      (
        ~is_first=false,
        ~is_last=false,
        start: CaretPosition.t,
        box: MeasuredLayout.box,
      ) =>
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col),
        y: Float.of_int(start.row) +. (is_first ? vtrim : 0.0),
      },
      width: Float.of_int(box.width),
      height:
        Float.of_int(box.height)
        -. (is_first ? vtrim : 0.0)
        -. (is_last ? vtrim : 0.0),
    };
  let (leading, last) = ListUtil.split_last(m.metrics);
  let (last_start, leading_rects) =
    leading
    |> List.mapi((i, box) => (i, box))
    |> ListUtil.map_with_accumulator(
         (start: CaretPosition.t, (i, box: MeasuredLayout.box)) =>
           (
             {row: start.row + box.height, col: 0},
             mk_rect(~is_first=i == 0, start, box),
           ),
         start,
       );
  let end_: CaretPosition.t = {
    row: last_start.row + last.height - 1,
    col:
      last.width
      + (
        switch (leading) {
        | [] => start.col
        | [_, ..._] => indent
        }
      ),
  };
  let last_rect =
    mk_rect(~is_first=leading == [], ~is_last=true, last_start, last);
  (end_, leading_rects @ [last_rect]);
};

let err_hole_view =
    (~corner_radii: (float, float), ~offset: int, subject: MeasuredLayout.t)
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["err-hole"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let var_err_hole_view =
    (~corner_radii: (float, float), ~offset: int, subject: MeasuredLayout.t)
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["var-err-hole"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let var_use_view =
    (~corner_radii: (float, float), ~offset: int, subject: MeasuredLayout.t)
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["var-use"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let tessera_rects =
    (start: CaretPosition.t, m: MeasuredLayout.t)
    : (CaretPosition.t, current_term_rects) => {
  let (end_, highlighted) = rects(start, m);
  let closed_children =
    m
    |> MeasuredLayout.flatten
    |> ListUtil.map_with_accumulator(
         (line_start, line: list(MeasuredLayout.t)) => {
           line
           |> ListUtil.map_with_accumulator(
                (word_start, word: MeasuredLayout.t) => {
                  switch (word) {
                  | {layout: Annot(ClosedChild({sort, _}), m), _} =>
                    let (word_end, rs) = rects(~vtrim=0.1, word_start, m);
                    (word_end, Some((sort, rs)));
                  | _ => (
                      MeasuredLayout.next_position(
                        ~indent=0,
                        word_start,
                        word,
                      ),
                      None,
                    )
                  }
                },
                line_start,
              )
           |> (
             fun
             | (line_end, rss) => (
                 line_end,
                 List.filter_map(sort_rs => sort_rs, rss),
               )
           )
         },
         start,
       )
    |> snd
    |> List.flatten;
  (end_, {highlighted, closed_children});
};

let inline_open_child_rects =
    (start: CaretPosition.t, m: MeasuredLayout.t): rects => {
  // TODO relax assumption
  assert(MeasuredLayout.height(m) == 1);
  // make singleton skinny rect along bottom
  [
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col),
        y: Float.of_int(start.row) +. 1. -. 0.1,
      },
      height: 0.1, // TODO
      width: Float.of_int(MeasuredLayout.width(m)),
    },
  ];
};

let multiline_open_child_rects =
    (~overflow_left, start: CaretPosition.t, m: MeasuredLayout.t): rects => {
  let overflow_left = overflow_left ? 0.25 : 0.0;
  [
    // make singleton skinny rect
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col) -. overflow_left,
        y: Float.of_int(start.row),
      },
      height: Float.of_int(MeasuredLayout.height(m)),
      width: 0.25 // TODO
    },
  ];
};

let line_rects =
    (
      ~overflow_left: bool,
      start: CaretPosition.t,
      line: list(MeasuredLayout.t),
    )
    : (CaretPosition.t, current_term_rects) => {
  let (end_, zipped) =
    line
    |> ListUtil.map_with_accumulator(
         (word_start, word: MeasuredLayout.t) => {
           switch (word) {
           | {layout: Annot(Tessera, m), _} =>
             let (word_end, {highlighted, closed_children}) =
               tessera_rects(word_start, m);
             (word_end, (highlighted, closed_children));
           | {layout: Annot(OpenChild({is_inline, _}), m), _} =>
             let highlighted =
               is_inline
                 ? inline_open_child_rects(word_start, m)
                 : multiline_open_child_rects(
                     ~overflow_left=true,
                     word_start,
                     m,
                   );
             let word_end =
               MeasuredLayout.next_position(~indent=0, word_start, m);
             (word_end, (highlighted, []));
           | _ =>
             failwith(
               "Doc nodes annotated as Term should only contain Tessera and OpenChild (flat) children",
             )
           }
         },
         start,
       );
  let (highlighted, closed_children) = List.split(zipped);
  let highlighted =
    switch (line) {
    | [{layout: Annot(Tessera, m), _}, ..._] when overflow_left =>
      let height = MeasuredLayout.height(m);
      [
        RectilinearPolygon.{
          min: {
            x: Float.of_int(start.col) -. 0.25,
            y: Float.of_int(start.row),
          },
          height: Float.of_int(height),
          width: 0.25,
        },
        ...List.flatten(highlighted),
      ];
    | _ => List.flatten(highlighted)
    };
  (end_, {highlighted, closed_children: List.flatten(closed_children)});
};

let lines_rects =
    (
      ~overflow_left: bool,
      start: CaretPosition.t,
      lines: list(list(MeasuredLayout.t)),
    )
    : (CaretPosition.t, current_term_rects) => {
  lines
  |> ListUtil.map_with_accumulator(
       (line_start, line: list(MeasuredLayout.t)) => {
         let (line_end, rss) =
           switch (line) {
           | [{layout: Annot(OpenChild(_), m), _}] =>
             let highlighted_rs =
               multiline_open_child_rects(~overflow_left, line_start, m);
             let line_end =
               MeasuredLayout.next_position(~indent=0, line_start, m);
             (line_end, (highlighted_rs, []));
           | _ =>
             let (line_end, {highlighted, closed_children}) =
               line_rects(~overflow_left, line_start, line);
             (line_end, (highlighted, closed_children));
           };
         ({row: line_end.row + 1, col: 0}, rss);
       },
       start,
     )
  |> (
    fun
    | (end_, zipped) => {
        let (highlighted_rs, closed_child_rss) = List.split(zipped);
        (
          {...end_, row: end_.row - 1},
          {
            highlighted: List.flatten(highlighted_rs),
            closed_children: List.flatten(closed_child_rss),
          },
        );
      }
  );
};

let subblock_rects =
    (~offset: int, subject: MeasuredLayout.t): current_term_rects => {
  let flattened = MeasuredLayout.flatten(subject);
  switch (flattened) {
  | [
      [{layout: Annot(Step(_), {layout: Annot(EmptyLine, _), _}), _}],
      ..._,
    ] =>
    // TODO undo hack
    {highlighted: [], closed_children: []}
  | _ =>
    flattened
    |> ListUtil.map_with_accumulator(
         (line_start, line: list(MeasuredLayout.t)) => {
           let (line_end, rss) =
             switch (line) {
             | [{layout: Annot(Step(_), m), _}] =>
               let (line_end, {highlighted, closed_children}) =
                 lines_rects(
                   ~overflow_left=true,
                   line_start,
                   MeasuredLayout.peel_and_flatten(m),
                 );
               (line_end, (highlighted, closed_children));
             | [{layout: Annot(OpenChild(_), m), _}] =>
               let highlighted =
                 multiline_open_child_rects(
                   ~overflow_left=true,
                   line_start,
                   m,
                 );
               let line_end =
                 MeasuredLayout.next_position(~indent=0, line_start, m);
               (line_end, (highlighted, []));
             | _ =>
               failwith(
                 "Doc nodes annotated as SubBlock should only contain *Line and OpenChild (flat) children",
               )
             };
           ({row: line_end.row + 1, col: 0}, rss);
         },
         {row: 0, col: offset},
       )
    |> (
      fun
      | (_, zipped) => {
          let (highlighted_rs, closed_child_rss) = List.split(zipped);
          {
            highlighted: List.flatten(highlighted_rs),
            closed_children: List.flatten(closed_child_rss),
          };
        }
    )
  };
};

let sort_cls: TermSort.t => string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp";

let closed_child_filter = (sort: TermSort.t) => {
  let sort_cls = sort_cls(sort);
  Vdom.(
    Node.create_svg(
      "filter",
      [
        Attr.id(
          String.lowercase_ascii(sort_cls) ++ "-closed-child-drop-shadow",
        ),
      ],
      [
        Node.create_svg(
          "feOffset",
          [
            Attr.create("in", "SourceAlpha"),
            Attr.create("dx", "0.1"),
            Attr.create("dy", "0.04"),
            Attr.create("result", "offset-alpha"),
          ],
          [],
        ),
        Node.create_svg(
          "feFlood",
          [
            Attr.classes(["closed-child-inset-shadow", sort_cls]),
            Attr.create("flood-opacity", "1"),
            Attr.create("result", "color"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          [
            // Attr.classes(["closed-child-drop-shadow"]),
            Attr.create("operator", "out"),
            Attr.create("in", "SourceAlpha"),
            Attr.create("in2", "offset-alpha"),
            Attr.create("result", "shadow-shape"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          [
            Attr.create("operator", "in"),
            Attr.create("in", "color"),
            Attr.create("in2", "shadow-shape"),
            Attr.create("result", "drop-shadow"),
          ],
          [],
        ),
        Node.create_svg(
          "feMerge",
          [],
          [
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "SourceGraphic")],
              [],
            ),
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "drop-shadow")],
              [],
            ),
          ],
        ),
      ],
    )
  );
};

let current_term_view =
    (
      ~corner_radii: (float, float),
      ~offset: int,
      ~sort: TermSort.t,
      ~shape: TermShape.t,
      subject: MeasuredLayout.t,
    )
    : Vdom.Node.t => {
  let {highlighted, closed_children} =
    switch (shape) {
    | SubBlock(_) =>
      // special case for now
      subblock_rects(~offset, subject)
    | Rule
    | Var(_)
    | Operand(_)
    | Invalid =>
      snd(
        lines_rects(
          ~overflow_left=false,
          {row: 0, col: offset},
          MeasuredLayout.flatten(subject),
        ),
      )
    | Case(_)
    | BinOp(_)
    | NTuple(_) =>
      snd(
        lines_rects(
          ~overflow_left=true,
          {row: 0, col: offset},
          MeasuredLayout.flatten(subject),
        ),
      )
    };

  let highlighted_vs =
    ListUtil.is_empty(highlighted)
      ? []
      : [
        RectilinearPolygon.mk_svg(
          ~corner_radii,
          ~attrs=Vdom.[Attr.classes(["code-current-term", sort_cls(sort)])],
          highlighted,
        ),
      ];
  let closed_child_vs =
    closed_children
    |> List.map(
         fun
         | (sort, rs) =>
           RectilinearPolygon.mk_svg(
             ~corner_radii,
             ~attrs=[
               Vdom.Attr.classes(["code-closed-child", sort_cls(sort)]),
             ],
             rs,
           ),
       );
  let outer_filter =
    Vdom.(
      Node.create_svg(
        "filter",
        [Attr.id("outer-drop-shadow")],
        [
          Node.create_svg(
            "feDropShadow",
            [
              Attr.classes(["current-term-drop-shadow", sort_cls(sort)]),
              Attr.create("dx", "0.1"),
              Attr.create("dy", "0.04"),
              Attr.create("stdDeviation", "0"),
            ],
            [],
          ),
        ],
      )
    );
  // <feOffset in="SourceGraphic" dx="60" dy="60" />
  // <feFlood flood-color="black" flood-opacity="1" result="color"/>
  Vdom.(
    Node.create_svg(
      "g",
      [],
      [
        // TODO cache filters at document root
        outer_filter,
        closed_child_filter(Typ),
        closed_child_filter(Pat),
        closed_child_filter(Exp),
        ...highlighted_vs,
      ]
      @ closed_child_vs,
    )
  );
};

let caret_view =
    (~font_metrics: FontMetrics.t, {row, col}: CaretPosition.t): Vdom.Node.t => {
  Vdom.(
    Node.span(
      [
        Attr.id("caret"),
        Attr.create(
          "style",
          Printf.sprintf(
            // TODO make more robust
            "top: calc(%fpx - 1px); left: calc(%fpx - 1px);",
            Float.of_int(row) *. font_metrics.row_height,
            Float.of_int(col) *. font_metrics.col_width,
          ),
        ),
        Attr.classes(["blink"]),
      ],
      [],
    )
  );
};

let view =
    (
      ~corner_radius=2.5, // px
      ~font_metrics: FontMetrics.t,
      // TODO document
      ~origin: CaretPosition.t,
      ~offset: int,
      // ~shape: TermShape.t,
      ~subject: MeasuredLayout.t,
      d: Decoration.t,
    ) => {
  let num_rows =
    subject.metrics
    |> List.map((box: MeasuredLayout.box) => box.height)
    |> List.fold_left((+), 0);
  let buffered_height = Float.of_int(num_rows + 1) *. font_metrics.row_height;

  let num_cols =
    List.tl(subject.metrics)
    |> List.map((box: MeasuredLayout.box) => box.width)
    |> List.fold_left(max, offset + List.hd(subject.metrics).width);
  let buffered_width = Float.of_int(num_cols + 1) *. font_metrics.col_width;

  let corner_radii = (
    corner_radius /. font_metrics.col_width,
    corner_radius /. font_metrics.row_height,
  );

  let v =
    switch (d) {
    | ErrHole => err_hole_view(~corner_radii, ~offset, subject)
    | VarErrHole => var_err_hole_view(~corner_radii, ~offset, subject)
    | VarUse => var_use_view(~corner_radii, ~offset, subject)
    | CurrentTerm(sort, shape) =>
      current_term_view(~corner_radii, ~offset, ~sort, ~shape, subject)
    };

  let cls =
    switch (d) {
    | ErrHole => "err-hole"
    | VarErrHole => "var-err-hole"
    | VarUse => "var-use"
    | CurrentTerm(_) => "current-term"
    };

  Vdom.(
    Node.div(
      [
        Attr.classes([
          "decoration-container",
          Printf.sprintf("%s-container", cls),
        ]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx - 1px); left: %fpx;",
            (Float.of_int(origin.row) -. 0.5) *. font_metrics.row_height,
            (Float.of_int(origin.col) -. 0.5) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        Node.create_svg(
          "svg",
          [
            Attr.classes([cls]),
            Attr.create(
              "viewBox",
              Printf.sprintf("-0.5 -0.5 %d %d", num_cols + 1, num_rows + 1),
            ),
            Attr.create("width", Printf.sprintf("%fpx", buffered_width)),
            Attr.create("height", Printf.sprintf("%fpx", buffered_height)),
            Attr.create("preserveAspectRatio", "none"),
          ],
          [v],
        ),
      ],
    )
  );
};
