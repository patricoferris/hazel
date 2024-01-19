type key =
  | H('a): key;

module Make = (MemoTbl: MemoTbl.S) => {
  open Types.Box;
  let height_tbl: MemoTbl.t(key, int) = MemoTbl.mk();

  let rec height: 'annot. t('annot) => int =
    box =>
      switch (MemoTbl.get(height_tbl, H(box))) {
      | Some(h) => h
      | None =>
        let h =
          switch (box) {
          | Text(_) => 1
          | Annot(_, b) => height(b)
          | HBox(bs) => bs |> List.map(height) |> List.fold_left(max, 1) // Note: 1 is HBox([]) height
          | VBox(bs) => bs |> List.map(height) |> List.fold_left((+), 0)
          };
        MemoTbl.set(height_tbl, H(box), h);
        h;
      };

  // Note: annots are inside-out (i.e. List.hd(annots) is the inner-most annot)
  let rec annot = (annots: list('annot), box: t('annot)): t('annot) => {
    switch (annots) {
    | [] => box
    | [ann, ...anns] => annot(anns, Annot(ann, box))
    };
  };

  let rec append_box:
    'annot.
    (~annots: list('annot)=?, t('annot), t('annot)) => t('annot)
   =
    (~annots=[], box1, box2) =>
      if (height(box1) <= 1) {
        HBox([annot(annots, box1), box2]);
      } else {
        let rec append_last = bs1 => {
          switch (bs1) {
          | [] => failwith("impossible due to `box_height` guard")
          | [b1] => [append_box(~annots, b1, box2)]
          | [b1, ...bs1] => [annot(annots, b1), ...append_last(bs1)]
          };
        };
        switch (box1) {
        | Text(_) => failwith("impossible due to `box_height` guard")
        | HBox(bs1) => HBox(append_last(bs1))
        | VBox(bs1) => VBox(append_last(bs1))
        | Annot(annot, b) => append_box(~annots=[annot, ...annots], b, box2)
        };
      };

  let append_hbox = (boxes1: list(t('annot)), boxes2: list(t('annot))) => {
    switch (ListUtil.split_last_opt(boxes1)) {
    | None => boxes2
    | Some((leading, last)) => leading @ [append_box(last, HBox(boxes2))]
    };
  };

  let table: ref(Hmap.t) = ref(Hmap.empty);

  let mk = (l: Layout.t('annot)): t('annot) => {
    open Types.Box;
    let mk = (boxes: list(list(t(_)))) =>
      VBox(List.map(row => HBox(row), boxes));
    let rec go: 'annot. Layout.t('annot) => List.t(List.t(t('annot))) =
      l => {
        switch (Hmap.find(Layout.key(l), table^)) {
        | Some(box) =>
          switch (box) {
          | Layout.Boxes(m) => m
          | _ => assert(false)
          }
        | None =>
          let box =
            switch (l.layout) {
            | Linebreak => [[], []]
            | Text(s) => [[Text(s)]]
            | Align(l) => [[mk(go(l))]]
            | Annot(ann, l) =>
              go(l) |> List.map(row => [Annot(ann, HBox(row))])
            | Cat(l1, l2) =>
              let (leading, last) = ListUtil.split_last(go(l1));
              let (first, trailing) = ListUtil.split_first(go(l2));
              leading @ [append_hbox(last, first), ...trailing];
            };
          table := Hmap.add(l.key, Layout.Boxes(box), table^);
          box;
        };
      };
    mk(go(l));
  };
};
