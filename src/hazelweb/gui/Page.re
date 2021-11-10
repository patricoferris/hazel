open Virtual_dom.Vdom;
open Node;
let logo_panel =
  a(
    [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
    [text("Hazel")],
  );

let top_bar = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  div(
    [Attr.classes(["top-bar"])],
    [
      logo_panel,
      CardsPanel.view(~inject, ~model),
      ActionMenu.view(~inject),
    ],
  );
};

let left_sidebar = (~inject: ModelAction.t => Event.t, ~model: Model.t) =>
  Sidebar.left(~inject, ~is_open=model.left_sidebar_open, () =>
    [ActionPanel.view(~inject, model)]
  );

let right_sidebar =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~assert_map: AssertMap.t,
    ) => {
  let program = Model.get_program(model);
  Sidebar.right(~inject, ~is_open=model.right_sidebar_open, () =>
    [
      AssertPanel.view(~inject, ~model, ~assert_map),
      ContextInspector.view(
        ~inject,
        ~selected_instance=Model.get_selected_hole_instance(model),
        ~settings=model.settings.evaluation,
        ~font_metrics=model.font_metrics,
        program,
      ),
      UndoHistoryPanel.view(~inject, model),
      SettingsPanel.view(~inject, model.settings),
    ]
  );
};

let git_panel = {
  let git_str =
    Printf.sprintf(
      "[%s @ %s (%s)]",
      Version_autogenerated.branch,
      Version_autogenerated.commit_hash_short,
      Version_autogenerated.commit_time,
    );
  span([Attr.class_("branch-panel")], [text(git_str)]);
};

let type_view = (ty: HTyp.t): Node.t => {
  let type_label =
    div([Attr.class_("type-label")], [text("Result of type: ")]);
  let type_view = div([Attr.class_("htype-view")], [HTypCode.view(ty)]);
  div(
    [Attr.class_("cell-status")],
    [div([Attr.class_("type-indicator")], [type_label, type_view])],
  );
};

let result_view = (~model: Model.t, ~inject, ~result: DHExp.t): Node.t =>
  div(
    [Attr.classes(["result-view"])],
    [
      DHCode.view(
        ~inject,
        ~selected_instance=Model.get_selected_hole_instance(model),
        ~font_metrics=model.font_metrics,
        ~settings=model.settings.evaluation,
        ~width=80,
        result,
      ),
    ],
  );

let status_view =
    (~model: Model.t, ~inject, ~result_ty: HTyp.t, ~result: DHExp.t): Node.t =>
  div(
    [],
    model.settings.evaluation.evaluate
      ? [type_view(result_ty), result_view(~model, ~inject, ~result)] : [],
  );

let page =
    (~inject, ~model: Model.t, ~result_ty: HTyp.t, ~result: DHExp.t): Node.t => {
  let card_caption =
    div(
      [Attr.class_("card-caption")],
      [Model.get_card(model).info.caption],
    );
  div(
    [Attr.class_("page")],
    [
      card_caption,
      Cell.view(~inject, model),
      status_view(~model, ~inject, ~result_ty, ~result),
    ],
  );
};

let run_program = (model: Model.t): (DHExp.t, HTyp.t, AssertMap.t) => {
  let program = Model.get_program(model);
  let (result, state) =
    model.settings.evaluation.show_unevaluated_elaboration
      ? (program |> Program.get_elaboration, EvalState.init)
      : program |> Program.get_result |> ((((r, _, _), s)) => (r, s));
  let (_, result_ty, _) = program.edit_state;
  (result, result_ty, state.assert_map);
};

let view = (~inject: ModelAction.t => Event.t, model: Model.t): Node.t => {
  let (result, result_ty, assert_map) = run_program(model);
  let page_area =
    div(
      [Attr.id("page-area")],
      [page(~inject, ~model, ~result_ty, ~result), git_panel],
    );
  let main_area =
    div(
      [Attr.class_("main-area")],
      [
        left_sidebar(~inject, ~model),
        div([Attr.classes(["flex-wrapper"])], [page_area]),
        right_sidebar(~inject, ~model, ~assert_map),
      ],
    );
  div([Attr.id("root")], [top_bar(~inject, ~model), main_area]);
};
