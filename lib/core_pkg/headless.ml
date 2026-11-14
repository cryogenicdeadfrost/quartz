(* WAI-ARIA Accessible Headless UI Primitives (`Quartz.Headless`).
   Zero-style, progressive-enhancement, keyboard-nav & ARIA-compliant building
   blocks mirroring Radix UI / React Aria without any JavaScript requirement for
   basic interactions (native <dialog>, Popover API, <details>/<summary>). *)

let dialog ~id ~title_id ?(open_ = false) ?(style = []) ?(attrs = []) kids =
  let base_attrs =
    [
      Dom.attr "role" "dialog";
      Dom.attr "aria-modal" "true";
      Dom.attr "aria-labelledby" title_id;
      Dom.attr "data-q-trap-focus" "1";
      Dom.bool_attr "open" open_;
    ]
    @ attrs
  in
  Dom.dialog ~id ~style ~attrs:base_attrs kids

let popover_trigger ~target_id ?(action = "toggle") ?(style = []) kids =
  Dom.button
    ~style
    ~attrs:
      [
        Dom.attr "popovertarget" target_id;
        Dom.attr "popovertargetaction" action;
        Dom.attr "aria-haspopup" "dialog";
        Dom.attr "aria-controls" target_id;
      ]
    kids

let popover_panel ~id ?(mode = "auto") ?(style = []) kids =
  Dom.div
    ~id
    ~style
    ~attrs:[ Dom.attr "popover" mode; Dom.attr "data-q-popover" "1" ]
    kids

let accordion_item ?(open_ = false) ~summary_style ~panel_style ~summary_kids panel_kids =
  Dom.details
    ~attrs:[ Dom.bool_attr "open" open_ ]
    [
      Dom.summary ~style:summary_style summary_kids;
      Dom.div ~style:panel_style panel_kids;
    ]

type tab_spec = {
  id : string;
  label : string;
  panel : unit -> Dom.node;
}

let tabs ~group_id ~active_id ?(bar_style = []) ?(tab_style = fun _ -> []) ?(panel_style = []) (items : tab_spec list) =
  let tab_buttons =
    List.mapi
      (fun idx it ->
        let is_active = it.id = active_id in
        let tab_el_id = Printf.sprintf "%s-tab-%s" group_id it.id in
        let panel_el_id = Printf.sprintf "%s-panel-%s" group_id it.id in
        Dom.button
          ~id:tab_el_id
          ~style:(tab_style is_active)
          ~attrs:
            [
              Dom.attr "role" "tab";
              Dom.attr "aria-selected" (if is_active then "true" else "false");
              Dom.attr "aria-controls" panel_el_id;
              Dom.attr "tabindex" (if is_active || (idx = 0 && active_id = "") then "0" else "-1");
            ]
          [ Dom.text it.label ])
      items
  in
  let tablist =
    Dom.div
      ~style:bar_style
      ~attrs:
        [
          Dom.attr "role" "tablist";
          Dom.attr "aria-orientation" "horizontal";
          Dom.attr "data-q-roving" "horizontal";
        ]
      tab_buttons
  in
  let panels =
    List.map
      (fun it ->
        let is_active = it.id = active_id in
        let tab_el_id = Printf.sprintf "%s-tab-%s" group_id it.id in
        let panel_el_id = Printf.sprintf "%s-panel-%s" group_id it.id in
        Dom.div
          ~id:panel_el_id
          ~style:panel_style
          ~attrs:
            [
              Dom.attr "role" "tabpanel";
              Dom.attr "aria-labelledby" tab_el_id;
              Dom.attr "tabindex" "0";
              Dom.bool_attr "hidden" (not is_active);
            ]
          (if is_active then [ it.panel () ] else []))
      items
  in
  Dom.div ~attrs:[ Dom.attr "data-q-tabs" group_id ] (tablist :: panels)

let menu ~id ?(aria_label = "Menu") ?(style = []) (items : (string * (unit -> unit) option) list) =
  Dom.ul
    ~id
    ~style
    ~attrs:
      [
        Dom.attr "role" "menu";
        Dom.attr "aria-label" aria_label;
        Dom.attr "data-q-roving" "vertical";
      ]
    (List.mapi
       (fun idx (lbl, on_select) ->
         let btn_attrs = [ Dom.attr "role" "menuitem"; Dom.attr "tabindex" (if idx = 0 then "0" else "-1") ] in
         Dom.li
           ~attrs:[ Dom.attr "role" "none" ]
           [ Dom.button ~attrs:btn_attrs ?on_click:on_select [ Dom.text lbl ] ])
       items)

let combobox ~id ~list_id ~value ?(expanded = false) ?(placeholder = "Search...") ?(style = []) ?on_input (options : (string * bool) list) =
  let input_el =
    Dom.input
      ~id
      ~value
      ~placeholder
      ?on_input
      ~style
      ~attrs:
        [
          Dom.attr "role" "combobox";
          Dom.attr "aria-autocomplete" "list";
          Dom.attr "aria-expanded" (if expanded then "true" else "false");
          Dom.attr "aria-controls" list_id;
          Dom.attr "aria-label" placeholder;
        ]
      ()
  in
  let listbox_el =
    Dom.ul
      ~id:list_id
      ~attrs:
        [
          Dom.attr "role" "listbox";
          Dom.bool_attr "hidden" (not expanded);
          Dom.attr "data-q-roving" "vertical";
        ]
      (List.map
         (fun (opt_lbl, selected) ->
           Dom.li
             ~attrs:
               [
                 Dom.attr "role" "option";
                 Dom.attr "aria-selected" (if selected then "true" else "false");
                 Dom.attr "tabindex" "-1";
               ]
             [ Dom.text opt_lbl ])
         options)
  in
  Dom.div ~attrs:[ Dom.attr "data-q-combobox" id ] [ input_el; listbox_el ]

let tooltip ~trigger_id ~tip_id ~text ?(style = []) trigger =
  Dom.div
    ~attrs:[ Dom.attr "data-q-tooltip-wrap" trigger_id ]
    [
      trigger;
      Dom.div
        ~id:tip_id
        ~style
        ~attrs:[ Dom.attr "role" "tooltip"; Dom.attr "aria-hidden" "true" ]
        [ Dom.text text ];
    ]

let live_toast ~variant:_ ?(style = []) text_msg =
  Dom.div
    ~style
    ~attrs:[ Dom.attr "role" "status"; Dom.attr "aria-live" "polite"; Dom.attr "aria-atomic" "true" ]
    [ Dom.text text_msg ]
