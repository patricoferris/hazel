open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
};

let off: t = {
  statics: false,
  elaborate: false,
  assist: false,
  dynamics: false,
};
let on: t = {statics: true, elaborate: true, assist: true, dynamics: true};
