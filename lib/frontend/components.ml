open Style

type variant =
  | Primary
  | Success
  | Warning
  | Danger
  | Neutral

let variant_colors = function
  | Primary -> (Palette.sky_500, Palette.sky_400, `Rgba (14, 165, 233, 0.15))
  | Success -> (Palette.emerald_500, Palette.emerald_400, `Rgba (16, 185, 129, 0.15))
  | Warning -> (Palette.amber_500, Palette.amber_400, `Rgba (245, 158, 11, 0.15))
  | Danger -> (Palette.rose_500, Palette.rose_400, `Rgba (244, 63, 94, 0.15))
  | Neutral -> (Palette.slate_700, Palette.slate_300, `Rgba (51, 65, 85, 0.4))

let badge ?(variant = Primary) txt =
  let border_c, text_c, bg_c = variant_colors variant in
  Dom.span
    ~style:
      [
        inline_block;
        px 10;
        py 2;
        rounded_full;
        text_xs;
        font_semibold;
        color text_c;
        bg bg_c;
        border ~width:1 ~color:border_c ();
      ]
    [ Dom.text txt ]

(* Contrast-aware pill badge: uses WCAG 2.1 relative luminance (`Contrast.pick_ink`)
   to automatically pick crisp dark or white text for any background color! *)
let contrast_badge ~bg_color txt =
  let ink = Contrast.pick_ink bg_color in
  let cr = Contrast.contrast_ratio ink bg_color in
  Dom.span
    ~style:
      [
        inline_block;
        px 10;
        py 4;
        rounded_full;
        text_xs;
        font_bold;
        bg bg_color;
        color ink;
      ]
    [ Dom.textf "%s (%.1f:1)" txt cr ]

let btn ?(variant = Primary) ?(disabled = false) ?on_click label_txt =
  let bg_base, hover_bg =
    match variant with
    | Primary -> (Palette.sky_400, Palette.sky_500)
    | Success -> (Palette.emerald_400, Palette.emerald_500)
    | Warning -> (Palette.amber_400, Palette.amber_500)
    | Danger -> (Palette.rose_600, Palette.rose_500)
    | Neutral -> (Palette.slate_800, Palette.slate_700)
  in
  let txt_col = Contrast.pick_ink bg_base in
  Dom.button
    ~disabled
    ?on_click
    ~style:
      [
        px 16;
        py 8;
        rounded_lg;
        font_semibold;
        text_sm;
        bg bg_base;
        color txt_col;
        border ~width:0 ();
        cursor_pointer;
        transition_all;
        hover [ bg hover_bg; translate_y (-1) ];
        active [ scale 0.98 ];
      ]
    ~label:label_txt
    []

let card ?title ?subtitle ?right ?footer ?(spotlight = false) ?(style = []) kids =
  let header_node =
    match title, subtitle, right with
    | None, None, None -> Dom.empty
    | _ ->
      Dom.div
        ~style:[ flex_row; justify_between; items_center; pb 12; mb 12; border_b ~color:Palette.slate_800 () ]
        [
          Dom.div
            ~style:[ flex_col; gap 4 ]
            [
              (match title with
               | Some t -> Dom.h3 ~style:[ text_lg; font_semibold; color Palette.white; m 0 ] [ Dom.text t ]
               | None -> Dom.empty);
              (match subtitle with
               | Some s -> Dom.span ~style:[ text_xs; color Palette.slate_400 ] [ Dom.text s ]
               | None -> Dom.empty);
            ];
          Option.value ~default:Dom.empty right;
        ]
  in
  let footer_node =
    match footer with
    | None -> Dom.empty
    | Some f ->
      Dom.div ~style:[ pt 12; mt 12; border_t ~color:Palette.slate_800 () ] [ f ]
  in
  let attrs = if spotlight then [ Dom.attr "data-q-spotlight" "1" ] else [] in
  Dom.div
    ~attrs
    ~style:
      ([
         flex_col;
         p 20;
         rounded_xl;
         bg Palette.slate_900;
         border ~width:1 ~color:Palette.slate_800 ();
         shadow_md;
         transition_all;
       ]
      @ style)
    ([ header_node ] @ kids @ [ footer_node ])

(* Interactive card with 60fps cursor-following radial spotlight & spring hover lift *)
let spotlight_card ?title ?subtitle ?right ?footer ?(style = []) kids =
  card ?title ?subtitle ?right ?footer ~spotlight:true ~style:(hover_lift :: style) kids

let stat_card ~label ~value ?delta ?(positive = true) () =
  let delta_node =
    match delta with
    | None -> Dom.empty
    | Some d -> badge ~variant:(if positive then Success else Danger) d
  in
  spotlight_card
    ~style:[ gap 8 ]
    [
      Dom.div
        ~style:[ flex_row; justify_between; items_center ]
        [
          Dom.span ~style:[ text_xs; uppercase; tracking_wide; color Palette.slate_400 ] [ Dom.text label ];
          delta_node;
        ];
      Dom.div ~style:[ text_3xl; font_bold; color Palette.white; font_mono ] [ Dom.text value ];
    ]

let alert ?(variant = Primary) ~title msg =
  let border_c, text_c, bg_c = variant_colors variant in
  Dom.div
    ~style:[ flex_col; gap 4; p 14; rounded_lg; bg bg_c; border ~width:1 ~color:border_c () ]
    [
      Dom.strong ~style:[ text_sm; font_semibold; color text_c ] [ Dom.text title ];
      Dom.span ~style:[ text_xs; color Palette.slate_200 ] [ Dom.text msg ];
    ]

let modal ~(open_sig : bool Signal.t) ~title ?footer body =
  Dom.show
    ~when_:(fun () -> Signal.get open_sig)
    (fun () ->
      Dom.div
        ~style:
          [
            fixed;
            inset_0;
            z_index 50;
            flex_col;
            items_center;
            justify_center;
            bg (`Rgba (2, 6, 23, 0.78));
            backdrop_blur 6;
            animate_fade_in;
          ]
        [
          card
            ~title
            ~right:(btn ~variant:Neutral ~on_click:(fun () -> Signal.set open_sig false) "Close")
            ?footer
            ~style:[ w_px 480; max_w 520; shadow_xl; animate_slide_up ]
            body;
        ])

(* Slide-over right-hand drawer sheet *)
let drawer ~(open_sig : bool Signal.t) ~title body =
  Dom.show
    ~when_:(fun () -> Signal.get open_sig)
    (fun () ->
      Dom.div
        ~style:[ fixed; inset_0; z_index 50; flex_row; justify_end; bg (`Rgba (2, 6, 23, 0.65)); backdrop_blur 4 ]
        [
          Dom.div
            ~style:
              [
                w_px 400;
                h_full;
                p 24;
                flex_col;
                gap 16;
                bg Palette.slate_900;
                border ~width:1 ~color:Palette.slate_800 ();
                shadow_xl;
              ]
            [
              Dom.div
                ~style:[ flex_row; justify_between; items_center; pb 14; border_b ~color:Palette.slate_800 () ]
                [
                  Dom.h3 ~style:[ text_lg; font_bold; m 0 ] [ Dom.text title ];
                  btn ~variant:Neutral ~on_click:(fun () -> Signal.set open_sig false) "✕";
                ];
              Dom.div ~style:[ flex_col; gap 12; overflow_y_auto ] body;
            ];
        ])

let tabs ~initial (items : (string * string * (unit -> Dom.node)) list) =
  let active_tab = Signal.make initial in
  Dom.rx (fun () ->
    let cur = Signal.get active_tab in
    let bar =
      Dom.div
        ~style:[ flex_row; gap 8; pb 10; mb 16; border_b ~color:Palette.slate_800 () ]
        (List.map
           (fun (id, lbl, _) ->
             let is_cur = id = cur in
             Dom.button
               ~label:lbl
               ~on_click:(fun () -> Signal.set active_tab id)
               ~style:
                 [
                   px 14;
                   py 6;
                   rounded_md;
                   text_sm;
                   font_medium;
                   cursor_pointer;
                   transition_all;
                   border ~width:0 ();
                   bg (if is_cur then Palette.sky_400 else Palette.slate_800);
                   color (if is_cur then Palette.slate_950 else Palette.slate_200);
                 ]
               [])
           items)
    in
    let panel =
      match List.find_opt (fun (id, _, _) -> id = cur) items with
      | Some (_, _, render_fn) -> render_fn ()
      | None -> Dom.empty
    in
    Dom.div ~style:[ flex_col; w_full ] [ bar; panel ])

(* Collapsible multi-item accordion *)
let accordion (sections : (string * string * Dom.node) list) =
  let open_id : string option Signal.t =
    match sections with (id, _, _) :: _ -> Signal.make (Some id) | [] -> Signal.make None
  in
  Dom.rx (fun () ->
    let cur = Signal.get open_id in
    Dom.div
      ~style:[ flex_col; gap 8; w_full ]
      (List.map
         (fun (id, title_txt, content) ->
           let is_open = cur = Some id in
           Dom.div
             ~style:[ flex_col; rounded_lg; bg Palette.slate_900; border ~width:1 ~color:Palette.slate_800 () ]
             [
               Dom.button
                 ~on_click:(fun () -> Signal.set open_id (if is_open then None else Some id))
                 ~style:
                   [
                     flex_row;
                     justify_between;
                     items_center;
                     w_full;
                     p 14;
                     bg Palette.transparent;
                     color Palette.white;
                     border ~width:0 ();
                     cursor_pointer;
                     font_semibold;
                     text_sm;
                   ]
                 [
                   Dom.span [ Dom.text title_txt ];
                   Dom.span ~style:[ color Palette.sky_400 ] [ Dom.text (if is_open then "−" else "+") ];
                 ];
               (if is_open then
                  Dom.div ~style:[ px 14; pb 14; text_sm; color Palette.slate_300 ] [ content ]
                else Dom.empty);
             ])
         sections))

(* Animated pill switch toggle *)
let switch_toggle ~label (sig_b : bool Signal.t) =
  Dom.rx (fun () ->
    let on = Signal.get sig_b in
    Dom.div
      ~style:[ flex_row; items_center; justify_between; gap 12 ]
      [
        Dom.span ~style:[ text_sm; color Palette.slate_200 ] [ Dom.text label ];
        Dom.button
          ~on_click:(fun () -> Signal.set sig_b (not on))
          ~style:
            [
              w_px 44;
              h_px 24;
              p 2;
              rounded_full;
              border ~width:0 ();
              cursor_pointer;
              transition_all;
              flex_row;
              items_center;
              (if on then justify_end else justify_start);
              bg (if on then Palette.emerald_500 else Palette.slate_700);
            ]
          [
            Dom.span ~style:[ w_px 20; h_px 20; rounded_full; bg Palette.white; shadow_sm ] [];
          ];
      ])

(* Fluid progress bar *)
let progress_bar ?(color = Palette.sky_500) ~label ~pct () =
  let clamped = Float.max 0.0 (Float.min 100.0 pct) in
  Dom.div
    ~style:[ flex_col; gap 6; w_full ]
    [
      Dom.div
        ~style:[ flex_row; justify_between; text_xs; Style.color Palette.slate_300 ]
        [ Dom.span [ Dom.text label ]; Dom.span ~style:[ font_mono ] [ Dom.textf "%.0f%%" clamped ] ];
      Dom.div
        ~style:[ w_full; h_px 8; rounded_full; bg Palette.slate_800; overflow_hidden ]
        [
          Dom.div ~style:[ w_pct clamped; h_full; rounded_full; bg color; transition_all ] [];
        ];
    ]

(* Interactive range slider bound to an int Signal.t *)
let slider ~label ~min ~max (sig_v : int Signal.t) =
  Dom.rx (fun () ->
    let cur = Signal.get sig_v in
    Dom.div
      ~style:[ flex_col; gap 6; w_full ]
      [
        Dom.div
          ~style:[ flex_row; justify_between; items_center ]
          [
            Dom.span ~style:[ text_sm; color Palette.slate_300 ] [ Dom.text label ];
            badge ~variant:Primary (string_of_int cur);
          ];
        Dom.input
          ~name:label
          ~type_:"range"
          ~min:(string_of_int min)
          ~max:(string_of_int max)
          ~value:(string_of_int cur)
          ~style:[ w_full; cursor_pointer ]
          ~on_change:(fun s ->
            match int_of_string_opt s with
            | Some n -> Signal.set sig_v n
            | None -> ())
          ();
      ])

(* Shimmer skeleton loader box *)
let skeleton ?(height = 20) ?(width_pct = 100.0) () =
  Dom.div
    ~style:[ w_pct width_pct; h_px height; rounded_md; bg Palette.slate_800; animate_pulse ]
    []

(* User avatar circle with status dot *)
let avatar ~initials ?(online = true) ?(bg_col = Palette.indigo_600) () =
  let ink = Contrast.pick_ink bg_col in
  Dom.div
    ~style:[ relative; inline_block; w_px 36; h_px 36 ]
    [
      Dom.div
        ~style:
          [
            w_px 36;
            h_px 36;
            rounded_full;
            bg bg_col;
            color ink;
            flex_row;
            items_center;
            justify_center;
            text_xs;
            font_bold;
            border ~width:2 ~color:Palette.slate_900 ();
          ]
        [ Dom.text initials ];
      Dom.span
        ~style:
          [
            absolute;
            bottom 0;
            right 0;
            w_px 10;
            h_px 10;
            rounded_full;
            bg (if online then Palette.emerald_400 else Palette.amber_400);
            border ~width:2 ~color:Palette.slate_900 ();
          ]
        [];
    ]

(* Vertical activity / deployment timeline *)
let timeline (items : (string * string * string) list) =
  Dom.div
    ~style:[ flex_col; gap 14; pl 8; border ~width:0 () ]
    (List.map
       (fun (time_lbl, title_txt, desc_txt) ->
         Dom.div
           ~style:[ flex_row; items_start; gap 12 ]
           [
             Dom.div
               ~style:
                 [
                   w_px 10;
                   h_px 10;
                   mt 6;
                   rounded_full;
                   bg Palette.sky_400;
                   shadow_glow Palette.sky_400;
                 ]
               [];
             Dom.div
               ~style:[ flex_col; gap 2 ]
               [
                 Dom.div
                   ~style:[ flex_row; items_center; gap 8 ]
                   [
                     Dom.strong ~style:[ text_sm; color Palette.white ] [ Dom.text title_txt ];
                     Dom.span ~style:[ text_xs; font_mono; color Palette.slate_400 ] [ Dom.text time_lbl ];
                   ];
                 Dom.span ~style:[ text_xs; color Palette.slate_300 ] [ Dom.text desc_txt ];
               ];
           ])
       items)

(* Multi-step wizard stepper *)
let stepper ~current_step (steps : string list) =
  Dom.div
    ~style:[ flex_row; items_center; justify_between; gap 8; w_full ]
    (List.mapi
       (fun i lbl ->
         let done_ = i < current_step in
         let active = i = current_step in
         let circle_bg =
           if done_ then Palette.emerald_400
           else if active then Palette.sky_400
           else Palette.slate_800
         in
         let circle_fg = Contrast.pick_ink circle_bg in
         Dom.div
           ~style:[ flex_row; items_center; gap 8 ]
           [
             Dom.span
               ~style:
                 [
                   w_px 24;
                   h_px 24;
                   rounded_full;
                   bg circle_bg;
                   color circle_fg;
                   text_xs;
                   font_bold;
                   flex_row;
                   items_center;
                   justify_center;
                 ]
               [ Dom.text (if done_ then "✓" else string_of_int (i + 1)) ];
             Dom.span
               ~style:[ text_xs; font_semibold; color (if active then Palette.white else Palette.slate_400) ]
               [ Dom.text lbl ];
           ])
       steps)

(* Interactive Kanban Board: click any task card to advance it across lanes! *)
type kanban_item = {
  id : int;
  title : string;
  tag : string;
  lane : string;
}

let kanban_board ~(lanes : string list) (tasks_sig : kanban_item list Signal.t) =
  let next_lane cur =
    let rec find = function
      | [] -> cur
      | [ _ ] -> List.hd lanes
      | x :: y :: rest -> if x = cur then y else find (y :: rest)
    in
    find lanes
  in
  let advance_task task_id =
    Signal.update tasks_sig
      (List.map (fun t -> if t.id = task_id then { t with lane = next_lane t.lane } else t))
  in
  Dom.rx (fun () ->
    let all_tasks = Signal.get tasks_sig in
    Dom.div
      ~style:[ grid_cols (max 1 (List.length lanes)); gap 14; w_full ]
      (List.map
         (fun lane_name ->
           let lane_tasks = List.filter (fun t -> t.lane = lane_name) all_tasks in
           Dom.div
             ~style:
               [
                 flex_col;
                 gap 10;
                 p 14;
                 rounded_xl;
                 bg Palette.slate_950;
                 border ~width:1 ~color:Palette.slate_800 ();
               ]
             [
               Dom.div
                 ~style:[ flex_row; justify_between; items_center; pb 8; border_b ~color:Palette.slate_800 () ]
                 [
                   Dom.strong ~style:[ text_xs; uppercase; tracking_wide; color Palette.slate_300 ]
                     [ Dom.text lane_name ];
                   badge ~variant:Neutral (string_of_int (List.length lane_tasks));
                 ];
               Dom.div
                 ~style:[ flex_col; gap 8 ]
                 (List.map
                    (fun t ->
                      Dom.div
                        ~style:
                          [
                            flex_col;
                            gap 6;
                            p 12;
                            rounded_lg;
                            bg Palette.slate_900;
                            border ~width:1 ~color:Palette.slate_800 ();
                            hover_lift;
                          ]
                        [
                          Dom.div
                            ~style:[ flex_row; justify_between; items_center ]
                            [
                              badge ~variant:Primary t.tag;
                              Dom.button
                                ~label:"Advance →"
                                ~on_click:(fun () -> advance_task t.id)
                                ~style:
                                  [
                                    px 8;
                                    py 2;
                                    rounded_sm;
                                    text_xs;
                                    bg Palette.slate_800;
                                    color Palette.sky_400;
                                    border ~width:0 ();
                                    cursor_pointer;
                                  ]
                                [];
                            ];
                          Dom.span ~style:[ text_sm; font_medium; color Palette.white ] [ Dom.text t.title ];
                        ])
                    lane_tasks);
             ])
         lanes))

let contains_sub hay needle =
  let hlen = String.length hay and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let ok = ref false in
    for i = 0 to hlen - nlen do
      if not !ok && String.sub hay i nlen = needle then ok := true
    done;
    !ok

(* ⌘K Command Palette modal with live search filtering *)
let command_palette ~(open_sig : bool Signal.t) (commands : (string * string * (unit -> unit)) list) =
  let query_sig = Signal.make "" in
  Dom.show
    ~when_:(fun () -> Signal.get open_sig)
    (fun () ->
      let q = String.lowercase_ascii (Signal.get query_sig) in
      let filtered =
        List.filter
          (fun (lbl, hint, _) ->
            q = "" || contains_sub (String.lowercase_ascii (lbl ^ " " ^ hint)) q)
          commands
      in
      Dom.div
        ~style:
          [
            fixed;
            inset_0;
            z_index 60;
            flex_col;
            items_center;
            pt 80;
            bg (`Rgba (2, 6, 23, 0.78));
            backdrop_blur 6;
          ]
        [
          card
            ~title:"⌘K Command Palette"
            ~right:(btn ~variant:Neutral ~on_click:(fun () -> Signal.set open_sig false) "ESC")
            ~style:[ w_px 500; max_w 540; shadow_xl; animate_slide_up ]
            [
              Dom.input
                ~placeholder:"Type a command or route..."
                ~value:(Signal.get query_sig)
                ~on_input:(fun v -> Signal.set query_sig v)
                ~style:
                  [
                    w_full;
                    p 12;
                    mb 12;
                    rounded_lg;
                    bg Palette.slate_950;
                    color Palette.white;
                    border ~width:1 ~color:Palette.sky_500 ();
                  ]
                ();
              Dom.div
                ~style:[ flex_col; gap 6 ]
                (List.map
                   (fun (lbl, hint, action) ->
                     Dom.button
                       ~on_click:(fun () ->
                         action ();
                         Signal.set open_sig false)
                       ~style:
                         [
                           flex_row;
                           justify_between;
                           items_center;
                           w_full;
                           p 12;
                           rounded_lg;
                           bg Palette.slate_950;
                           color Palette.white;
                           border ~width:1 ~color:Palette.slate_800 ();
                           cursor_pointer;
                           hover [ bg Palette.slate_800 ];
                         ]
                       [
                         Dom.span ~style:[ text_sm; font_semibold ] [ Dom.text lbl ];
                         Dom.span ~style:[ text_xs; font_mono; color Palette.sky_400 ] [ Dom.text hint ];
                       ])
                   filtered);
            ];
        ])

(* Reactive Toast Notification System *)
type toast_msg = {
  id : int;
  title : string;
  variant : variant;
}

let active_toasts : toast_msg list Signal.t = Signal.make []
let next_toast_id = ref 1

let push_toast ?(variant = Success) title =
  let id = !next_toast_id in
  incr next_toast_id;
  let cur = Signal.peek active_toasts in
  let trimmed = List.filteri (fun i _ -> i < 3) cur in
  Signal.set active_toasts ({ id; title; variant } :: trimmed)

let dismiss_toast id =
  Signal.update active_toasts (List.filter (fun (t : toast_msg) -> t.id <> id))

let toast_rack () =
  Dom.rx (fun () ->
    let items = Signal.get active_toasts in
    if items = [] then Dom.empty
    else
      Dom.div
        ~style:[ fixed; bottom 20; right 20; z_index 60; flex_col; gap 10; w_px 320 ]
        (List.map
           (fun (t : toast_msg) ->
             let border_c, text_c, _ = variant_colors t.variant in
             Dom.div
               ~style:
                 [
                   flex_row;
                   justify_between;
                   items_center;
                   p 14;
                   rounded_xl;
                   glass;
                   border ~width:1 ~color:border_c ();
                   shadow_xl;
                   animate_slide_up;
                 ]
               [
                 Dom.span ~style:[ text_sm; font_semibold; color text_c ] [ Dom.text t.title ];
                 Dom.button
                   ~label:"✕"
                   ~on_click:(fun () -> dismiss_toast t.id)
                   ~style:[ bg Palette.transparent; color Palette.slate_400; border ~width:0 (); cursor_pointer ]
                   [];
               ])
           items))

let navbar ~brand ?right (links : (string * string) list) =
  Dom.header
    ~style:
      [
        sticky;
        top 0;
        z_index 40;
        flex_row;
        justify_between;
        items_center;
        px 28;
        py 14;
        glass;
        border_b ~color:Palette.slate_800 ();
      ]
    [
      Dom.div
        ~style:[ flex_row; items_center; gap 28 ]
        [
          Dom.span ~style:[ text_xl; font_bold; color Palette.sky_400 ] [ Dom.text brand ];
          Dom.nav
            ~style:[ flex_row; gap 18 ]
            (List.map
               (fun (href, lbl) ->
                 Router.link ~to_:href ~style:[ text_sm; color Palette.slate_300 ] [ Dom.text lbl ])
               links);
        ];
      Option.value ~default:Dom.empty right;
    ]

let data_table ~columns ~rows =
  Dom.div
    ~style:[ w_full; overflow_x_auto; rounded_lg; border ~width:1 ~color:Palette.slate_800 () ]
    [
      Dom.table
        ~style:[ w_full; raw_css "border-collapse" "collapse"; text_sm ]
        [
          Dom.thead
            ~style:[ bg Palette.slate_900 ]
            [
              Dom.tr
                (List.map
                   (fun (col_title, _) ->
                     Dom.th
                       ~style:[ p 12; text_left; font_semibold; color Palette.slate_400; border_b ~color:Palette.slate_800 () ]
                       [ Dom.text col_title ])
                   columns);
            ];
          Dom.tbody
            (List.map
               (fun r ->
                 Dom.tr
                   ~style:[ hover [ bg Palette.slate_800 ]; border_b ~color:Palette.slate_800 (); transition_all ]
                   (List.map
                      (fun (_, render_cell) ->
                        Dom.td ~style:[ p 12; color Palette.slate_200 ] [ render_cell r ])
                      columns))
               rows);
        ];
    ]

let code_block ~lang code_str =
  Dom.div
    ~style:[ flex_col; rounded_lg; bg Palette.slate_950; border ~width:1 ~color:Palette.slate_800 (); overflow_hidden ]
    [
      Dom.div
        ~style:[ flex_row; justify_between; items_center; px 12; py 6; bg Palette.slate_900; border_b ~color:Palette.slate_800 () ]
        [
          Dom.span ~style:[ text_xs; font_mono; color Palette.sky_400 ] [ Dom.text lang ];
          Dom.span ~style:[ text_xs; color Palette.slate_500 ] [ Dom.text "OCaml Native" ];
        ];
      Dom.pre ~style:[ p 14; m 0; font_mono; text_xs; color Palette.slate_200; overflow_x_auto ] [ Dom.code [ Dom.text code_str ] ];
    ]

let sparkline = Charts.sparkline
let area_chart = Charts.area_chart
let bar_chart = Charts.bar_chart
let donut_ring = Charts.donut_ring
let multi_line_chart = Charts.multi_line_chart
let heatmap = Charts.heatmap
let radar_chart = Charts.radar_chart
let candlestick_chart = Charts.candlestick_chart
