open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: Model.settings,
      ~result: ModelResult.simple,
    ) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  let code_id = "code-container";
  let editor_view =
    Cell.editor_with_result_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=["single"],
      ~selected=true,
      ~mousedown,
      ~code_id,
      ~settings,
      ~info_map,
      ~result,
      editor,
    );
  let ci_view =
    settings.statics
      ? [CursorInspector.view(~inject, ~settings, zipper, info_map)] : [];
  let bottom_bar = [div(~attr=Attr.class_("bottom-bar"), ci_view)];

  div(~attr=clss(["editor", "single"]), [editor_view] @ bottom_bar);
};

let download_slide_state = state => {
  let data = Export.export_scratchpad(state);
  Export.download_json("hazel-scratchpad", data);
};

let toolbar_buttons = (~inject, state: ScratchSlide.state) => {
  let export_button =
    Widgets.button(
      Icons.export,
      _ => {
        download_slide_state(state);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Scratchpad",
    );
  let import_button =
    Widgets.file_select_button(
      "import-scratchpad",
      Icons.export, // TODO import icon
      file => {
        switch (file) {
        | None => Virtual_dom.Vdom.Effect.Ignore
        | Some(file) => inject(UpdateAction.InitiateScratchpadImport(file))
        }
      },
      ~tooltip="Import Scratchpad",
    );
  [export_button, import_button];
};
