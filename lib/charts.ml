(* Pure-OCaml SVG visualization engine.
   Zero D3/Chart.js bloat — compiles straight to vector nodes. *)

let next_grad_id = ref 1

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
    let last_x, last_y = List.nth pts (n - 1) in
    Dom.svg
      ~view_box:(Printf.sprintf "0 0 %d %d" width height)
      ~width:(Printf.sprintf "%dpx" width)
      ~height:(Printf.sprintf "%dpx" height)
      [
        Dom.svg_polyline ~points:pts ~stroke ~stroke_width:2.2 ();
        Dom.svg_circle ~cx:last_x ~cy:last_y ~r:3.5 ~fill:stroke ();
      ]

(* Full area chart with linear-gradient underfill, dashed grid lines, and min/max labels *)
let area_chart ?(width = 480) ?(height = 150) ?(color = "#38bdf8") (values : float list) =
  match values with
  | [] | [ _ ] -> Dom.empty
  | _ ->
    incr next_grad_id;
    let gid = Printf.sprintf "q_area_grad_%d" !next_grad_id in
    let min_v = List.fold_left Float.min Float.infinity values in
    let max_v = List.fold_left Float.max Float.neg_infinity values in
    let span = Float.max 1e-6 (max_v -. min_v) in
    let n = List.length values in
    let wf = float_of_int width and hf = float_of_int height in
    let pad_t = 14.0 and pad_b = 14.0 in
    let plot_h = hf -. pad_t -. pad_b in
    let pts =
      List.mapi
        (fun i v ->
          let x = (float_of_int i /. float_of_int (n - 1)) *. wf in
          let norm = (v -. min_v) /. span in
          let y = pad_t +. plot_h -. (norm *. plot_h) in
          (x, y))
        values
    in
    let poly_pts = (0.0, hf -. pad_b) :: pts @ [ (wf, hf -. pad_b) ] in
    let last_x, last_y = List.nth pts (n - 1) in
    Dom.svg
      ~view_box:(Printf.sprintf "0 0 %d %d" width height)
      ~width:"100%"
      ~height:(Printf.sprintf "%dpx" height)
      [
        Dom.svg_defs
          [
            Dom.svg_linear_gradient ~id:gid
              [
                Dom.svg_stop ~offset:"0%" ~color ~opacity:0.42 ();
                Dom.svg_stop ~offset:"100%" ~color ~opacity:0.0 ();
              ];
          ];
        Dom.svg_line ~x1:0.0 ~y1:pad_t ~x2:wf ~y2:pad_t ~stroke:"#1e293b" ~stroke_dasharray:"4 4" ();
        Dom.svg_line ~x1:0.0 ~y1:(pad_t +. (plot_h /. 2.0)) ~x2:wf ~y2:(pad_t +. (plot_h /. 2.0)) ~stroke:"#1e293b" ~stroke_dasharray:"4 4" ();
        Dom.svg_line ~x1:0.0 ~y1:(hf -. pad_b) ~x2:wf ~y2:(hf -. pad_b) ~stroke:"#1e293b" ();
        Dom.svg_polygon ~points:poly_pts ~fill:(Printf.sprintf "url(#%s)" gid) ();
        Dom.svg_polyline ~points:pts ~stroke:color ~stroke_width:2.5 ();
        Dom.svg_circle ~cx:last_x ~cy:last_y ~r:4.0 ~fill:color ();
        Dom.svg_text ~x:6.0 ~y:11.0 ~fill:"#64748b" (Printf.sprintf "max %.1f" max_v);
        Dom.svg_text ~x:6.0 ~y:(hf -. 3.0) ~fill:"#64748b" (Printf.sprintf "min %.1f" min_v);
      ]

let bar_chart ?(width = 340) ?(height = 140) ?(fill = "#38bdf8") (bars : (string * float) list) =
  match bars with
  | [] -> Dom.empty
  | _ ->
    let max_v = List.fold_left (fun acc (_, v) -> Float.max acc v) 1e-6 bars in
    let n = List.length bars in
    let wf = float_of_int width and hf = float_of_int height in
    let slot_w = wf /. float_of_int n in
    let bar_w = Float.max 8.0 (slot_w *. 0.62) in
    let nodes =
      List.concat
        (List.mapi
           (fun i (lbl, v) ->
             let h = Float.max 4.0 ((v /. max_v) *. (hf -. 34.0)) in
             let x = (float_of_int i *. slot_w) +. ((slot_w -. bar_w) /. 2.0) in
             let y = hf -. 20.0 -. h in
             let cx = x +. (bar_w /. 2.0) in
             [
               Dom.svg_rect ~x ~y ~width:bar_w ~height:h ~rx:4.0 ~fill ();
               Dom.svg_text ~x:cx ~y:(hf -. 5.0) ~fill:"#94a3b8" ~font_size:10 ~anchor:"middle" lbl;
             ])
           bars)
    in
    Dom.svg
      ~view_box:(Printf.sprintf "0 0 %d %d" width height)
      ~width:"100%"
      ~height:(Printf.sprintf "%dpx" height)
      nodes

(* Circular SVG ring gauge using stroke-dasharray circumference math *)
let donut_ring ?(size = 110) ?(color = "#10b981") ~pct ~label () =
  let pct_clamped = Float.max 0.0 (Float.min 100.0 pct) in
  let sf = float_of_int size in
  let cx = sf /. 2.0 and cy = sf /. 2.0 in
  let r = (sf /. 2.0) -. 10.0 in
  let circum = 2.0 *. Float.pi *. r in
  let filled = circum *. (pct_clamped /. 100.0) in
  let dash = Printf.sprintf "%.2f %.2f" filled circum in
  Dom.div
    ~style:Style.[ flex_col; items_center; gap 6 ]
    [
      Dom.svg
        ~view_box:(Printf.sprintf "0 0 %d %d" size size)
        ~width:(Printf.sprintf "%dpx" size)
        ~height:(Printf.sprintf "%dpx" size)
        [
          Dom.svg_circle ~cx ~cy ~r ~fill:"none" ~stroke:"#1e293b" ~stroke_width:8.0 ();
          Dom.svg_circle
            ~cx
            ~cy
            ~r
            ~fill:"none"
            ~stroke:color
            ~stroke_width:8.0
            ~stroke_dasharray:dash
            ~stroke_dashoffset:(circum *. 0.25)
            ();
          Dom.svg_text
            ~x:cx
            ~y:(cy +. 5.0)
            ~fill:"#f8fafc"
            ~font_size:15
            ~anchor:"middle"
            (Printf.sprintf "%.0f%%" pct_clamped);
        ];
      Dom.span ~style:Style.[ text_xs; font_medium; color Palette.slate_400 ] [ Dom.text label ];
    ]

(* Multi-series SVG line chart with automatic legend *)
let multi_line_chart ?(width = 480) ?(height = 140) (series : (string * string * float list) list) =
  let all_vals = List.concat_map (fun (_, _, vs) -> vs) series in
  match all_vals with
  | [] -> Dom.empty
  | _ ->
    let min_v = List.fold_left Float.min Float.infinity all_vals in
    let max_v = List.fold_left Float.max Float.neg_infinity all_vals in
    let span = Float.max 1e-6 (max_v -. min_v) in
    let wf = float_of_int width and hf = float_of_int height in
    let pad = 10.0 in
    let lines =
      List.filter_map
        (fun (_, col, vs) ->
          let n = List.length vs in
          if n < 2 then None
          else
            let pts =
              List.mapi
                (fun i v ->
                  let x = (float_of_int i /. float_of_int (n - 1)) *. wf in
                  let norm = (v -. min_v) /. span in
                  let y = hf -. pad -. (norm *. (hf -. (2.0 *. pad))) in
                  (x, y))
                vs
            in
            Some (Dom.svg_polyline ~points:pts ~stroke:col ~stroke_width:2.2 ()))
        series
    in
    let legend =
      Dom.div
        ~style:Style.[ flex_row; gap 16; mb 8 ]
        (List.map
           (fun (name, col, _) ->
             Dom.div
               ~style:Style.[ flex_row; items_center; gap 6 ]
               [
                 Dom.span ~style:Style.[ w_px 10; h_px 10; rounded_full; bg (`Hex col) ] [];
                 Dom.span ~style:Style.[ text_xs; color Palette.slate_300 ] [ Dom.text name ];
               ])
           series)
    in
    Dom.div
      ~style:Style.[ flex_col; w_full ]
      [
        legend;
        Dom.svg
          ~view_box:(Printf.sprintf "0 0 %d %d" width height)
          ~width:"100%"
          ~height:(Printf.sprintf "%dpx" height)
          lines;
      ]

(* GitHub-style intensity heatmap matrix *)
let heatmap ?(cols = 12) (values : float list) =
  let max_v = List.fold_left Float.max 1e-6 values in
  let cell_color v =
    let ratio = v /. max_v in
    if ratio <= 0.05 then "#0f172a"
    else if ratio <= 0.30 then "#0369a1"
    else if ratio <= 0.60 then "#0ea5e9"
    else if ratio <= 0.85 then "#38bdf8"
    else "#34d399"
  in
  Dom.div
    ~style:Style.[ grid_cols cols; gap 4 ]
    (List.map
       (fun v ->
         Dom.div
           ~style:
             Style.[
               h_px 18;
               rounded_sm;
               bg (`Hex (cell_color v));
               border ~width:1 ~color:Palette.slate_800 ();
               transition_all;
               hover [ scale 1.18 ];
             ]
           [])
       values)
