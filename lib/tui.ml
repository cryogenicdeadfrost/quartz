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

let extract_layout_props (styles : Style.t list) =
  let dir = ref Col in
  let pad = ref 0 in
  let gap = ref 0 in
  let fixed_w = ref None in
  let fixed_h = ref None in
  let has_border = ref false in
  List.iter
    (function
      | Style.Kv ("display", v) when String.length v >= 18 && String.sub v 0 18 = "flex;flex-directio" ->
        if String.contains v 'r' && not (String.contains v 'c') then dir := Row else dir := Col
      | Style.Kv ("padding", v) ->
        (match int_of_string_opt (String.sub v 0 (max 0 (String.length v - 2))) with
         | Some n -> pad := max 0 (n / 8)
         | None -> ())
      | Style.Kv ("gap", v) ->
        (match int_of_string_opt (String.sub v 0 (max 0 (String.length v - 2))) with
         | Some n -> gap := max 0 (n / 8)
         | None -> ())
      | Style.Kv ("width", v) when String.ends_with ~suffix:"px" v ->
        (match int_of_string_opt (String.sub v 0 (String.length v - 2)) with
         | Some n -> fixed_w := Some (max 4 (n / 8))
         | None -> ())
      | Style.Kv ("height", v) when String.ends_with ~suffix:"px" v ->
        (match int_of_string_opt (String.sub v 0 (String.length v - 2)) with
         | Some n -> fixed_h := Some (max 1 (n / 16))
         | None -> ())
      | Style.Kv ("border", _) -> has_border := true
      | _ -> ())
    styles;
  (!dir, !pad, !gap, !fixed_w, !fixed_h, !has_border)

let rec node_text = function
  | Dom.Empty -> ""
  | Dom.Text s | Dom.Raw_html s -> s
  | Dom.Fragment kids ->
    String.concat " " (List.filter (( <> ) "") (List.map node_text kids))
  | Dom.Dynamic fn -> node_text (fn ())
  | Dom.El { kids; _ } ->
    String.concat " " (List.filter (( <> ) "") (List.map node_text kids))

let compute_layout ?(width = 80) ?(height = 24) (root : Dom.node) : bbox list =
  let boxes = ref [] in
  let rec layout x y avail_w avail_h = function
    | Dom.Empty -> (0, 0)
    | Dom.Text s | Dom.Raw_html s ->
      let w = min avail_w (String.length s) in
      boxes := { x; y; w; h = 1; tag = "#text"; text = s } :: !boxes;
      (w, 1)
    | Dom.Dynamic fn -> layout x y avail_w avail_h (fn ())
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
    | Dom.El { tag; style; kids; _ } ->
      let dir, pad, gap, fw, fh, _ = extract_layout_props style in
      let w = match fw with Some w0 -> min avail_w w0 | None -> avail_w in
      let inner_x = x + pad in
      let inner_y = y + pad in
      let inner_w = max 1 (w - (2 * pad)) in
      (* leaf text shortcut for buttons/headings *)
      let only_text =
        List.for_all
          (function Dom.Text _ | Dom.Dynamic _ -> true | _ -> false)
          kids
      in
      if only_text && kids <> [] then (
        let txt = node_text (Dom.Fragment kids) in
        let disp = if tag = "button" then "[ " ^ txt ^ " ]" else txt in
        let h = Option.value ~default:1 fh in
        boxes := { x; y; w = min w (String.length disp + (2 * pad)); h; tag; text = disp } :: !boxes;
        (min w (String.length disp + (2 * pad)), h)
      ) else (
        let cx = ref inner_x in
        let cy = ref inner_y in
        let row_h = ref 1 in
        let n_kids = max 1 (List.length kids) in
        let col_w = max 4 ((inner_w - ((n_kids - 1) * max 1 gap)) / n_kids) in
        List.iter
          (fun k ->
            match dir with
            | Col ->
              let _, kh = layout !cx !cy inner_w avail_h k in
              cy := !cy + kh + max 0 gap
            | Row ->
              let kw, kh = layout !cx !cy col_w avail_h k in
              if kh > !row_h then row_h := kh;
              cx := !cx + max kw col_w + max 1 gap)
          kids;
        let used_h =
          match fh with
          | Some h0 -> h0
          | None ->
            (match dir with
             | Col -> max 1 (!cy - y + pad)
             | Row -> !row_h + (2 * pad))
        in
        boxes := { x; y; w; h = used_h; tag; text = "" } :: !boxes;
        (w, used_h)
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
    for i = 0 to String.length s - 1 do
      put_char (x + i) y s.[i]
    done
  in
  (* top & bottom frame border *)
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
