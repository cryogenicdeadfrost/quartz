(* DevTools Custom Formatter Registry & Inspector Dashboard (`Quartz.Debug_registry`).
   Enables human-readable inspection of OCaml variant constructors and record
   fields in Chrome DevTools, plus an HTML/JSON `/__quartz/inspector` dashboard.
   Zero Unix dependency. *)

type type_entry =
  | Variant_type of { name : string; constructors : string array }
  | Record_type of { name : string; fields : string array }

let registry : (string, type_entry) Hashtbl.t = Hashtbl.create 32

let register_variant ~type_name ~(constructors : string list) =
  Hashtbl.replace registry type_name
    (Variant_type { name = type_name; constructors = Array.of_list constructors })

let register_record ~type_name ~(fields : string list) =
  Hashtbl.replace registry type_name
    (Record_type { name = type_name; fields = Array.of_list fields })

let to_json () : Json.t =
  let items =
    Hashtbl.fold
      (fun _ entry acc ->
        let j =
          match entry with
          | Variant_type { name; constructors } ->
            Json.obj
              [
                ("kind", Json.string "variant");
                ("name", Json.string name);
                ("labels", Json.array (Array.to_list (Array.map Json.string constructors)));
              ]
          | Record_type { name; fields } ->
            Json.obj
              [
                ("kind", Json.string "record");
                ("name", Json.string name);
                ("labels", Json.array (Array.to_list (Array.map Json.string fields)));
              ]
        in
        ( (match entry with Variant_type v -> v.name | Record_type r -> r.name), j ) :: acc)
      registry []
  in
  Json.obj items

let emit_devtools_table_script () : string =
  "window.__QUARTZ_DEBUG_TYPES__ = " ^ Json.to_string (to_json ()) ^ ";"

let tag_value ~type_name (value_repr : Json.t) : Json.t =
  Json.obj
    [
      ("__quartzType", Json.string type_name);
      ("value", value_repr);
    ]

let render_inspector_html
    ~(title : string)
    ~(engine : string)
    ~(uptime_s : float)
    ~(requests_handled : int)
    ~(routes : Router.route_def list)
    ~(api_endpoints : string list)
    ~(recent_events : Pubsub.event list)
    ~(access_logs : string list)
    ~(registered_plugs : (string * string) list) : string =
  let stat_card lbl val_s =
    Dom.div
      ~style:
        Style.[
          p 16; rounded_lg; bg Palette.slate_900; border ~width:1 ~color:Palette.slate_800 ();
          flex_col; gap 4;
        ]
      [
        Dom.span ~style:Style.[ text_xs; font_mono; color Palette.slate_400 ] [ Dom.text lbl ];
        Dom.strong ~style:Style.[ text_xl; color Palette.sky_400 ] [ Dom.text val_s ];
      ]
  in
  let route_rows =
    List.map
      (fun (r : Router.route_def) ->
        Dom.li
          ~style:Style.[ py 6; border ~width:1 ~color:Palette.slate_800 (); px 10; rounded_md; font_mono; text_xs ]
          [ Dom.text (Printf.sprintf "%s  (%s)" r.pattern (Option.value ~default:"Untitled" r.title)) ])
      routes
  in
  let plug_rows =
    if registered_plugs = [] then
      [ Dom.li ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "No external npm plugs registered on this build." ] ]
    else
      List.map
        (fun (pkg, ver) ->
          Dom.li ~style:Style.[ py 4; font_mono; text_xs; color Palette.emerald_400 ]
            [ Dom.text (Printf.sprintf "%s@%s" pkg ver) ])
        registered_plugs
  in
  let log_rows =
    if access_logs = [] then
      [ Dom.li ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "No logged requests yet." ] ]
    else
      List.map
        (fun line ->
          Dom.li ~style:Style.[ py 4; font_mono; text_xs; color Palette.slate_300 ] [ Dom.text line ])
        access_logs
  in
  let event_rows =
    if recent_events = [] then
      [ Dom.li ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "No PubSub events emitted yet." ] ]
    else
      List.map
        (fun (ev : Pubsub.event) ->
          Dom.li ~style:Style.[ py 4; font_mono; text_xs; color Palette.sky_400 ]
            [ Dom.text (Printf.sprintf "#%d [%s] %s" ev.id ev.topic ev.payload) ])
        recent_events
  in
  let body =
    Dom.div
      ~style:Style.[ p 28; max_w 1120; mx_auto; flex_col; gap 20 ]
      [
        Dom.div ~style:Style.[ flex_row; justify_between; items_center ] [
          Dom.h1 ~style:Style.[ text_2xl; bold; m 0; color Palette.white ]
            [ Dom.text (title ^ " -- Runtime Inspector") ];
          Dom.span ~style:Style.[ px 10; py 4; rounded_md; bg Palette.emerald_600; color Palette.white; text_xs; font_mono ]
            [ Dom.text engine ];
        ];
        Dom.div ~style:Style.[ grid_cols 4; gap 12 ] [
          stat_card "Uptime" (Printf.sprintf "%.2fs" uptime_s);
          stat_card "Requests" (string_of_int requests_handled);
          stat_card "Routes" (string_of_int (List.length routes));
          stat_card "API Endpoints" (string_of_int (List.length api_endpoints));
        ];
        Dom.div ~style:Style.[ grid_cols 2; gap 16 ] [
          Dom.div ~style:Style.[ p 16; rounded_lg; bg Palette.slate_900; border ~width:1 ~color:Palette.slate_800 () ] [
            Dom.h2 ~style:Style.[ text_base; bold; m 0; mb 10 ] [ Dom.text "Registered Routes" ];
            Dom.ul ~style:Style.[ flex_col; gap 6; p 0 ] route_rows;
          ];
          Dom.div ~style:Style.[ p 16; rounded_lg; bg Palette.slate_900; border ~width:1 ~color:Palette.slate_800 () ] [
            Dom.h2 ~style:Style.[ text_base; bold; m 0; mb 10 ] [ Dom.text "Registered NPM Plugs & PubSub Events" ];
            Dom.ul ~style:Style.[ flex_col; gap 6; p 0 ] plug_rows;
            Dom.ul ~style:Style.[ flex_col; gap 6; p 0; mt 10 ] event_rows;
          ];
        ];
        Dom.div ~style:Style.[ p 16; rounded_lg; bg Palette.slate_900; border ~width:1 ~color:Palette.slate_800 () ] [
          Dom.h2 ~style:Style.[ text_base; bold; m 0; mb 10 ] [ Dom.text "Structured JSON Access Logs" ];
          Dom.ul ~style:Style.[ flex_col; gap 4; p 0 ] log_rows;
        ];
      ]
  in
  let doc, _ = App.render_full_document ~title:(title ^ " Inspector") ~live:false body in
  doc
