type t('annot) = Types.MeasuredLayout.t('annot);

/**
 * A measured layout coupled with the offset of its head box
 */
type with_offset('annot) = (int, t('annot));

let height: t(_) => int;
let width: (~offset: int=?, t(_)) => int;

/**
 * A standard fold operator except that the client is given
 * direct control over the recursion whenever it encounters
 * an `Annot` node, allowing pruning of irrelevant parts of
 * the tree as needed for performance.
 */
let fold:
  (
    ~linebreak: 'acc,
    ~text: string => 'acc,
    ~align: 'acc => 'acc,
    ~cat: ('acc, 'acc) => 'acc,
    // let client control recursion based on annotation
    ~annot: (t('annot) => 'acc, 'annot, t('annot)) => 'acc,
    t('annot)
  ) =>
  'acc;

/**
 * Same as `fold` but additionally exposes the `MeasuredPosition.t`
 * marking the start of each node
 */
let pos_fold:
  (
    ~linebreak: MeasuredPosition.t => 'acc,
    ~text: (MeasuredPosition.t, string) => 'acc,
    ~align: (MeasuredPosition.t, 'acc) => 'acc,
    ~cat: (MeasuredPosition.t, 'acc, 'acc) => 'acc,
    // let client control recursion based on annotation
    ~annot: (
              ~go: t('annot) => 'acc,
              ~indent: int,
              ~start: MeasuredPosition.t,
              'annot,
              t('annot)
            ) =>
            'acc,
    ~indent: int=?,
    ~start: MeasuredPosition.t=?,
    t('annot)
  ) =>
  'acc;

/**
 * `next_position(~indent, start, m)` returns the position at the
 * end of `m` assuming its starting position is `start` and `m` is
 * indented by `indent`.
 */
let next_position:
  (~indent: int, MeasuredPosition.t, t(_)) => MeasuredPosition.t;

module Make: (MemoTbl.S) => {
                              let mk: Layout.t('annot) => t('annot);
                            };
