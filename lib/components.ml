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

let btn ?(variant = Primary) ?(disabled = false) ?on_click label_txt =
  let bg_base, hover_bg, txt_col =
    match variant with
    | Primary -> (Palette.sky_500, Palette.sky_600, Palette.white)
    | Success -> (Palette.emerald_500, Palette.emerald_600, Palette.white)
    | Warning -> (Palette.amber_500, Palette.amber_400, Palette.slate_950)
    | Danger -> (Palette.rose_500, Palette.rose_600, Palette.white)
    | Neutral -> (Palette.slate_800, Palette.slate_700, Palette.slate_100)
  in
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
        hover [ bg hover_bg ];
      ]
    ~label:label_txt
    []

let card ?title ?subtitle ?right ?footer ?(style = []) kids =
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
  Dom.div
    ~style:
      ([
         flex_col;
         p 20;
         rounded_xl;
         bg Palette.slate_900;
         border ~width:1 ~color:Palette.slate_800 ();
         shadow_md;
       ]
      @ style)
    ([ header_node ] @ kids @ [ footer_node ])

let stat_card ~label ~value ?delta ?(positive = true) () =
  let delta_node =
    match delta with
    | None -> Dom.empty
    | Some d ->
      badge ~variant:(if positive then Success else Danger) d
  in
  card
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
            bg (`Rgba (2, 6, 23, 0.75));
            backdrop_blur 4;
          ]
        [
          card
            ~title
            ~right:(btn ~variant:Neutral ~on_click:(fun () -> Signal.set open_sig false) "Close")
            ?footer
            ~style:[ w_px 480; max_w 92; shadow_xl ]
            body;
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
                   border ~width:0 ();
                   bg (if is_cur then Palette.sky_500 else Palette.slate_800);
                   color (if is_cur then Palette.white else Palette.slate_300);
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

let navbar ~brand ?right (links : (string * string) list) =
  Dom.header
    ~style:
      [
        flex_row;
        justify_between;
        items_center;
        px 24;
        py 14;
        bg Palette.slate_900;
        border_b ~color:Palette.slate_800 ();
      ]
    [
      Dom.div
        ~style:[ flex_row; items_center; gap 24 ]
        [
          Dom.span ~style:[ text_xl; font_bold; color Palette.sky_400 ] [ Dom.text brand ];
          Dom.nav
            ~style:[ flex_row; gap 16 ]
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
                   ~style:[ hover [ bg Palette.slate_800 ]; border_b ~color:Palette.slate_800 () ]
                   (List.map
                      (fun (_, render_cell) ->
                        Dom.td ~style:[ p 12; color Palette.slate_200 ] [ render_cell r ])
                      columns))
               rows);
        ];
    ]

(* pure SVG sparkline so you don't need 400KB of Chart.js just to draw a trendline *)
let sparkline ?(width = 240) ?(height = 56) ?(stroke = "#38bdf8") (values : float list) =
  match values with
  | [] | [ _ ] -> Dom.empty
  | _ ->
    let min_v = List.fold_left Float.min Float.infinity values in
    let max_v = List.fold_left Float.max Float.neg_infinity values in
    let span = Float.max 1e-6 (max_v -. min_v) in
    let n = List.length values in
    let wf = float_of_int width and hf = float_of_int height in
    let pad = 4.0 in
    let pts =
      List.mapi
        (fun i v ->
          let x = (float_of_int i /. float_of_int (n - 1)) *. wf in
          let norm = (v -. min_v) /. span in
          let y = hf -. pad -. (norm *. (hf -. (2.0 *. pad))) in
          (x, y))
        values
    in
    Dom.svg
      ~view_box:(Printf.sprintf "0 0 %d %d" width height)
      ~width:(Printf.sprintf "%dpx" width)
      ~height:(Printf.sprintf "%dpx" height)
      [ Dom.svg_polyline ~points:pts ~stroke ~stroke_width:2.2 () ]

let bar_chart ?(width = 320) ?(height = 120) ?(fill = "#38bdf8") (bars : (string * float) list) =
  match bars with
  | [] -> Dom.empty
  | _ ->
    let max_v = List.fold_left (fun acc (_, v) -> Float.max acc v) 1e-6 bars in
    let n = List.length bars in
    let wf = float_of_int width and hf = float_of_int height in
    let slot_w = wf /. float_of_int n in
    let bar_w = Float.max 4.0 (slot_w *. 0.7) in
    let rects =
      List.mapi
        (fun i (_, v) ->
          let h = Float.max 2.0 ((v /. max_v) *. (hf -. 12.0)) in
          let x = (float_of_int i *. slot_w) +. ((slot_w -. bar_w) /. 2.0) in
          let y = hf -. h in
          Dom.svg_rect ~x ~y ~width:bar_w ~height:h ~rx:3.0 ~fill ())
        bars
    in
    Dom.svg
      ~view_box:(Printf.sprintf "0 0 %d %d" width height)
      ~width:(Printf.sprintf "%dpx" width)
      ~height:(Printf.sprintf "%dpx" height)
      rects
