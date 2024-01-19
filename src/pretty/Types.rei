module Box: {
  /**
 * A decomposition of `Layout` into box shapes that can be easily
 * translated into HTML boxes. In the process of converting an
 * annotated multiline `Layout` node into a `Box`, the `Layout`
 * node is split into three boxes (head, body, tail) with the
 * annotation distributed over them. For example, a layout node
 * of the form
 * (
 *   let x = 1 in
 *   x + 1
 * )
 * is split into
 * ---
 * |(|
 * -----------------
 * |  let x = 1 in |
 * |  x + 1        |
 * -----------------
 * |)|
 * ---
 * at the top level (each box may be further decomposed internally),
 * where each of the three depicted boxes is annotated with the
 * same annotation as the original layout node.
 */
  [@deriving sexp]
  type t('annot) =
    | Text(string)
    | HBox(list(t('annot)))
    | VBox(list(t('annot)))
    | Annot('annot, t('annot));
};

module MeasuredLayout: {
  [@deriving sexp]
  type box = {
    height: int,
    width: int,
  };

  /**
 * Augmented version of `Layout.t` where each layout node is
 * accompanied by metrics about its shape. These metrics may
 * be used, e.g., to generate SVG elements to decorate nodes.
 *
 * Each node shape is defined as a vertically stacked list of
 * boxes, left-aligned except for possibly the first.
 * The offset of the head box of a layout node from the
 * left-alignment axis of the tail boxes depends on the layout
 * nodes that come prior in a pre-order traversal of the overall
 * layout. For example, the layout node
 * (
 *   x + 1
 * )
 * has metrics `[{h: 1, w: 1}, {h: 1, w: 7}, {h: 1, w: 1}]` but
 * its head box offset varies with its context, e.g., the offset
 * would be `0` if the node is isolated as depicted above, or it
 * would be `8` if embedded in the layout below:
 * let y = (
 *   x + 1
 * ) in _
 */
  [@deriving sexp]
  type t('annot) = {
    layout: t'('annot),
    metrics: list(box),
  }
  and t'('annot) =
    | Linebreak
    | Text(string)
    | Align(t('annot))
    | Cat(t('annot), t('annot))
    | Annot('annot, t('annot));
};