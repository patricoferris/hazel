let debug_update = ref(false);
let debug_zipper = ref(false);
let debug_keystoke = ref(false);

[@deriving (show({with_path: false}), yojson)]
type entry = {
  update: Update.t,
  error: option(Update.Failure.t),
  timestamp: Model.timestamp,
  zipper: Printer.t,
};

[@deriving (show({with_path: false}), yojson)]
type updates = list(entry);

let mut_log: ref(updates) = ref([]); // TODO replace with mutable vec

let is_action_logged: Update.t => bool =
  fun
  | UpdateDoubleTap(_)
  | Mousedown
  | Mouseup
  | Save
  | SetFontMetrics(_)
  | SetLogoFontMetrics(_)
  | SetShowBackpackTargets(_)
  | UpdateResult(_) => false
  | Set(_)
  | LoadDefault
  | ToggleMode
  | SwitchSlide(_)
  | SwitchEditor(_)
  | PerformAction(_)
  | FailedInput(_)
  | Copy
  | Paste
  | Undo
  | Redo
  | MoveToNextHole(_) => true;

let is_keystroke_logged: Key.t => bool = _ => true;

let mk_entry = (~measured, update, z, error): entry => {
  let error =
    switch (error) {
    | Ok(_) => None
    | Error(failure) => Some(failure)
    };
  {
    zipper: Printer.of_zipper(~measured, z),
    update,
    error,
    timestamp: JsUtil.timestamp(),
  };
};

let to_string = (entry: entry) => {
  let status =
    switch (entry.error) {
    | None => "SUCCESS"
    | Some(failure) => "FAILURE(" ++ Update.Failure.show(failure) ++ ")"
    };
  Printf.sprintf(
    "%.0f: %s %s",
    entry.timestamp,
    Update.show(entry.update),
    status,
  );
};

[@deriving (show({with_path: false}), yojson)]
type key_entry = {
  key: Key.t,
  updates: list(Update.t),
  timestamp: Model.timestamp,
};

let mk_key_entry = (key, updates): key_entry => {
  {key, updates, timestamp: JsUtil.timestamp()};
};

let key_entry_to_string = ({key, updates, timestamp}: key_entry) => {
  let updates = updates |> List.map(Update.show) |> String.concat(", ");
  Printf.sprintf("%.0f: %s -> [%s]", timestamp, Key.to_string(key), updates);
};

let updates_of_string: string => updates =
  str =>
    switch (str |> Yojson.Safe.from_string |> updates_of_yojson) {
    | updates => updates
    | exception exc =>
      print_endline(
        "log: json decoding (2) exception: "
        ++ Printexc.to_string(exc)
        ++ "\n",
      );
      [];
    };
let json_update_log_key = "JSON_UPDATE_LOG";
let get_json_update_log = () =>
  switch (LocalStorage.get_localstore(json_update_log_key)) {
  | None => []
  | Some(str) => updates_of_string(str)
  };

let get_json_update_log_string = () =>
  get_json_update_log() |> yojson_of_updates |> Yojson.Safe.to_string;

let reset_json_log = () => {
  mut_log := [];
  LocalStorage.set_localstore(json_update_log_key, "");
};

let append_json_updates_log = () => {
  let new_updates = mut_log^;
  mut_log := [];
  let old_log = get_json_update_log();
  let new_log = new_updates @ old_log;
  let blah = Yojson.Safe.to_string(yojson_of_updates(new_log));
  LocalStorage.set_localstore(json_update_log_key, blah);
};

let update = (update: Update.t, old_model: Model.t, res) => {
  if (is_action_logged(update)) {
    let cur_model =
      switch (res) {
      | Ok(model) => model
      | Error(_) => old_model
      };
    let zip = Editors.get_zipper(cur_model.editors);
    let measured = Editors.get_editor(cur_model.editors).state.meta.measured;
    let new_entry = mk_entry(~measured, update, zip, res);
    mut_log := List.cons(new_entry, mut_log^);
    if (debug_update^) {
      let update_str = to_string(mk_entry(~measured, update, zip, res));
      print_endline(update_str);
      //new_entry |> entry_to_yojson |> Yojson.Safe.to_string |> print_endline;
    };
    if (debug_zipper^) {
      cur_model.editors
      |> Editors.get_zipper
      |> Printer.to_string_log(~measured)
      |> print_endline;
    };
  };
  res;
};

let keystroke = (key: Key.t, updates) => {
  if (is_keystroke_logged(key)) {
    if (debug_keystoke^) {
      let keystroke_str = key_entry_to_string(mk_key_entry(key, updates));
      print_endline(keystroke_str);
    };
  };
  updates;
};