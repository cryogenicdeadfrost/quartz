(* Pure-OCaml Flexbox layout solver + ANSI terminal renderer for Quartz views.
   Lets the exact same Dom.node tree render in a terminal window. *)

type bbox = {
  x : int;
  y : int;
  w : int;
  h : int;
  tag : string;
  text : string;
}

type direction =
  | Row
  | Col

let has_prefix ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let has_suffix ~suffix s =
  let slen = String.length suffix and len = String.length s in
  len >= slen && String.sub s (len - slen) slen = suffix

let extract_layout_props (styles : Style.t list) =
  let dir = ref Col in
  let pad = ref 0 in
  let gap = ref 0 in
  let fixed_w = ref None in
  let fixed_h = ref None in
  List.iter
    (function
      | Style.Kv ("display", v) ->
        if has_suffix ~suffix:":row" v || has_prefix ~prefix:"grid;" v then dir := Row
        else if has_suffix ~suffix:":column" v then dir := Col
      | Style.Kv ("padding", v) ->
        (match int_of_string_opt (String.sub v 0 (max 0 (String.length v - 2))) with
         | Some n -> pad := if n >= 16 then 1 else 0
         | None -> ())
      | Style.Kv ("gap", v) ->
        (match int_of_string_opt (String.sub v 0 (max 0 (String.length v - 2))) with
         | Some n -> gap := if n >= 8 then 1 else 0
         | None -> ())
      | Style.Kv ("width", v) when has_suffix ~suffix:"px" v ->
        (match int_of_string_opt (String.sub v 0 (String.length v - 2)) with
         | Some n -> fixed_w := Some (max 4 (n / 8))
         | None -> ())
      | Style.Kv ("height", v) when has_suffix ~suffix:"px" v ->
        (match int_of_string_opt (String.sub v 0 (String.length v - 2)) with
         | Some n -> fixed_h := Some (max 1 (n / 16))
         | None -> ())
      | _ -> ())
    styles;
  (!dir, !pad, !gap, !fixed_w, !fixed_h)

let rec unwrap_node = function
  | Dom.Dynamic fn -> unwrap_node (fn ())
  | n -> n

let rec node_text n =
  match unwrap_node n with
  | Dom.Empty -> ""
  | Dom.Text s | Dom.Raw_html s | Dom.Raw_unsafe s -> s
  | Dom.Fragment kids ->
    String.concat " " (List.filter (( <> ) "") (List.map node_text kids))
  | Dom.El { kids; _ } ->
    String.concat " " (List.filter (( <> ) "") (List.map node_text kids))
  | Dom.Dynamic _ -> assert false

let is_inline_leaf n =
  match unwrap_node n with
  | Dom.Text _ | Dom.Raw_html _ | Dom.Raw_unsafe _ -> true
  | Dom.El { tag = "button" | "a" | "span" | "strong" | "code" | "h1" | "h2" | "h3" | "p" | "th" | "td"; _ } -> true
  | _ -> false

let compute_layout ?(width = 80) ?(height = 24) (root : Dom.node) : bbox list =
  let boxes = ref [] in
  let rec layout x y avail_w avail_h raw_node =
    match unwrap_node raw_node with
    | Dom.Empty -> (0, 0)
    | Dom.Text s | Dom.Raw_html s | Dom.Raw_unsafe s ->
      let w = min avail_w (String.length s) in
      boxes := { x; y; w; h = 1; tag = "#text"; text = s } :: !boxes;
      (w, 1)
    | Dom.Dynamic _ -> assert false
    | Dom.Fragment kids ->
      let cy = ref y in
      let max_w = ref 0 in
      List.iter
        (fun k ->
          let kw, kh = layout x !cy avail_w (max 1 (avail_h - (!cy - y))) k in
          if kw > !max_w then max_w := kw;
          cy := !cy + kh)
        kids;
      (!max_w, !cy - y)
    | Dom.El { tag = "svg"; _ } ->
      let label = "[ ~~~~ SVG Chart ~~~~ ]" in
      boxes := { x; y; w = min avail_w (String.length label); h = 1; tag = "svg"; text = label } :: !boxes;
      (min avail_w (String.length label), 1)
    | Dom.El { tag; style; kids; _ } ->
      let dir, pad, gap, fw, fh = extract_layout_props style in
      let dir = if tag = "tr" || tag = "nav" || tag = "header" then Row else dir in
      let w = match fw with Some w0 -> min avail_w w0 | None -> avail_w in
      if is_inline_leaf (Dom.El { tag; id = None; cls = None; style; attrs = []; kids }) then (
        let txt = node_text (Dom.Fragment kids) in
        if txt = "" then (0, 0)
        else
          let disp = if tag = "button" then "[ " ^ txt ^ " ]" else txt in
          let bw = min w (String.length disp) in
          boxes := { x; y; w = bw; h = 1; tag; text = disp } :: !boxes;
          (bw, 1)
      ) else (
        let inner_x = x + pad in
        let inner_y = y + pad in
        let inner_w = max 1 (w - (2 * pad)) in
        let visible_kids =
          List.filter
            (fun k -> match unwrap_node k with Dom.Empty -> false | _ -> true)
            kids
        in
        match dir with
        | Col ->
          let cy = ref inner_y in
          List.iter
            (fun k ->
              let _, kh = layout inner_x !cy inner_w avail_h k in
              if kh > 0 then cy := !cy + kh + gap)
            visible_kids;
          let total_h =
            match fh with
            | Some h0 -> h0
            | None -> max 1 (!cy - y - (if visible_kids <> [] then gap else 0) + pad)
          in
          (w, total_h)
        | Row ->
          let n_kids = max 1 (List.length visible_kids) in
          let cx = ref inner_x in
          let max_kh = ref 1 in
          let slot_w = max 6 (inner_w / n_kids) in
          List.iter
            (fun k ->
              let kw, kh = layout !cx inner_y slot_w avail_h k in
              if kh > !max_kh then max_kh := kh;
              let step = if kw > 0 then kw + 2 else slot_w in
              cx := !cx + step)
            visible_kids;
          (w, !max_kh + (2 * pad))
      )
  in
  let _ = layout 0 0 width height root in
  List.rev !boxes

let render_tui ?(width = 76) ?(height = 20) (root : Dom.node) : string =
  let grid = Array.make_matrix height width ' ' in
  let put_char x y c =
    if x >= 0 && x < width && y >= 0 && y < height then grid.(y).(x) <- c
  in
  let put_str x y s =
    let col = ref x in
    for i = 0 to String.length s - 1 do
      let code = Char.code s.[i] in
      if code >= 32 && code <= 126 then (
        put_char !col y s.[i];
        incr col
      )
    done
  in
  for cx = 0 to width - 1 do
    put_char cx 0 '-';
    put_char cx (height - 1) '-'
  done;
  for cy = 0 to height - 1 do
    put_char 0 cy '|';
    put_char (width - 1) cy '|'
  done;
  put_char 0 0 '+';
  put_char (width - 1) 0 '+';
  put_char 0 (height - 1) '+';
  put_char (width - 1) (height - 1) '+';

  let boxes = compute_layout ~width:(width - 4) ~height:(height - 2) root in
  List.iter
    (fun (b : bbox) ->
      if b.text <> "" then put_str (b.x + 2) (b.y + 1) b.text)
    boxes;
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Buffer.add_char buf grid.(y).(x)
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf
