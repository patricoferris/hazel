open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {step: int};

let initial: t = {step: 0};

let take_step = ({step}) => {step: step + 1};
let get_step = ({step}) => step;
