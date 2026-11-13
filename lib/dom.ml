type key_event = {
  key : string;
  ctrl : bool;
  shift : bool;
  alt : bool;
}

type handler =
  | On_click of (unit -> unit)
  | On_input of (string -> unit)
  | On_change of (string -> unit)
  | On_check of (bool -> unit)
  | On_keydown of (key_event -> unit)
  | On_submit of ((string * string) list -> unit)

type attr =
  | Attr of string * string
  | Bool_attr of string * bool
  | Handler of handler

type node =
  | Empty
  | Text of string
  | Raw_html of string
  | Fragment of node list
  | El of {
      tag : string;
      id : string option;
      cls : string option;
      style : Style.t list;
      attrs : attr list;
      kids : node list;
    }
  | Dynamic of (unit -> node)

let attr k v = Attr (k, v)
let bool_attr k b = Bool_attr (k, b)
let on_click fn = Handler (On_click fn)
let on_input fn = Handler (On_input fn)
let on_change fn = Handler (On_change fn)
let on_check fn = Handler (On_check fn)
let on_keydown fn = Handler (On_keydown fn)
let on_submit fn = Handler (On_submit fn)

(* 1-liner two-way binding so controlled inputs don't need 6 lines of boilerplate *)
let bind_input (sig_v : string Signal.t) =
  [ Attr ("value", Signal.get sig_v); Handler (On_input (fun s -> Signal.set sig_v s)) ]

let bind_bool (sig_b : bool Signal.t) =
  [ Bool_attr ("checked", Signal.get sig_b); Handler (On_check (fun b -> Signal.set sig_b b)) ]

let empty = Empty
let text s = Text s
let textf fmt = Printf.ksprintf (fun s -> Text s) fmt
let raw_html s = Raw_html s
let fragment kids = Fragment kids
let rx fn = Dynamic fn
let rx_text fn = Dynamic (fun () -> Text (fn ()))

let show ~when_ ?(fallback = fun () -> Empty) render =
  Dynamic (fun () -> if when_ () then render () else fallback ())

let each ~items ~key:_ render_item =
  Dynamic (fun () -> Fragment (List.mapi (fun i x -> render_item x i) (items ())))

let el tag ?id ?class_ ?(style = []) ?(attrs = []) kids =
  El { tag; id; cls = class_; style; attrs; kids }

(* semantic containers *)
let div ?id ?class_ ?style ?attrs kids = el "div" ?id ?class_ ?style ?attrs kids
let span ?id ?class_ ?style ?attrs kids = el "span" ?id ?class_ ?style ?attrs kids
let main ?id ?class_ ?style ?attrs kids = el "main" ?id ?class_ ?style ?attrs kids
let section ?id ?class_ ?style ?attrs kids = el "section" ?id ?class_ ?style ?attrs kids
let article ?id ?class_ ?style ?attrs kids = el "article" ?id ?class_ ?style ?attrs kids
let header ?id ?class_ ?style ?attrs kids = el "header" ?id ?class_ ?style ?attrs kids
let footer ?id ?class_ ?style ?attrs kids = el "footer" ?id ?class_ ?style ?attrs kids
let nav ?id ?class_ ?style ?attrs kids = el "nav" ?id ?class_ ?style ?attrs kids
let aside ?id ?class_ ?style ?attrs kids = el "aside" ?id ?class_ ?style ?attrs kids
let details ?id ?class_ ?style ?attrs kids = el "details" ?id ?class_ ?style ?attrs kids
let summary ?id ?class_ ?style ?attrs kids = el "summary" ?id ?class_ ?style ?attrs kids
let dialog ?id ?class_ ?style ?attrs kids = el "dialog" ?id ?class_ ?style ?attrs kids

(* typography *)
let h1 ?id ?class_ ?style ?attrs kids = el "h1" ?id ?class_ ?style ?attrs kids
let h2 ?id ?class_ ?style ?attrs kids = el "h2" ?id ?class_ ?style ?attrs kids
let h3 ?id ?class_ ?style ?attrs kids = el "h3" ?id ?class_ ?style ?attrs kids
let h4 ?id ?class_ ?style ?attrs kids = el "h4" ?id ?class_ ?style ?attrs kids
let h5 ?id ?class_ ?style ?attrs kids = el "h5" ?id ?class_ ?style ?attrs kids
let h6 ?id ?class_ ?style ?attrs kids = el "h6" ?id ?class_ ?style ?attrs kids
let p ?id ?class_ ?style ?attrs kids = el "p" ?id ?class_ ?style ?attrs kids
let code ?id ?class_ ?style ?attrs kids = el "code" ?id ?class_ ?style ?attrs kids
let pre ?id ?class_ ?style ?attrs kids = el "pre" ?id ?class_ ?style ?attrs kids
let blockquote ?id ?class_ ?style ?attrs kids = el "blockquote" ?id ?class_ ?style ?attrs kids
let strong ?id ?class_ ?style ?attrs kids = el "strong" ?id ?class_ ?style ?attrs kids
let em ?id ?class_ ?style ?attrs kids = el "em" ?id ?class_ ?style ?attrs kids
let small ?id ?class_ ?style ?attrs kids = el "small" ?id ?class_ ?style ?attrs kids
let kbd ?id ?class_ ?style ?attrs kids = el "kbd" ?id ?class_ ?style ?attrs kids
let hr ?style () = el "hr" ?style []
let br () = el "br" []

(* interactive controls *)
let button ?id ?class_ ?(style = []) ?(disabled = false) ?(type_ = "button") ?on_click ?label kids =
  let a = ref [ Attr ("type", type_); Bool_attr ("disabled", disabled) ] in
  (match on_click with Some fn -> a := Handler (On_click fn) :: !a | None -> ());
  let children =
    match label with
    | Some s -> Text s :: kids
    | None -> kids
  in
  el "button" ?id ?class_ ~style ~attrs:!a children

let a ~href ?(target = "_self") ?id ?class_ ?style ?on_click kids =
  let at = ref [ Attr ("href", href); Attr ("target", target) ] in
  (match on_click with Some fn -> at := Handler (On_click fn) :: !at | None -> ());
  el "a" ?id ?class_ ?style ~attrs:!at kids

let form ?id ?class_ ?style ?on_submit kids =
  let at = match on_submit with Some fn -> [ Handler (On_submit fn) ] | None -> [] in
  el "form" ?id ?class_ ?style ~attrs:at kids

let input
    ?id
    ?name
    ?(type_ = "text")
    ?value
    ?placeholder
    ?(disabled = false)
    ?(required = false)
    ?style
    ?(attrs = [])
    ?on_input
    ?on_change
    ?on_keydown
    () =
  let at = ref (Attr ("type", type_) :: Bool_attr ("disabled", disabled) :: Bool_attr ("required", required) :: attrs) in
  (match name with Some n -> at := Attr ("name", n) :: !at | None -> ());
  (match value with Some v -> at := Attr ("value", v) :: !at | None -> ());
  (match placeholder with Some p -> at := Attr ("placeholder", p) :: !at | None -> ());
  (match on_input with Some fn -> at := Handler (On_input fn) :: !at | None -> ());
  (match on_change with Some fn -> at := Handler (On_change fn) :: !at | None -> ());
  (match on_keydown with Some fn -> at := Handler (On_keydown fn) :: !at | None -> ());
  el "input" ?id ?style ~attrs:!at []

let textarea ?id ?name ?(rows = 4) ?value ?placeholder ?style ?on_input () =
  let at = ref [ Attr ("rows", string_of_int rows) ] in
  (match name with Some n -> at := Attr ("name", n) :: !at | None -> ());
  (match placeholder with Some p -> at := Attr ("placeholder", p) :: !at | None -> ());
  (match on_input with Some fn -> at := Handler (On_input fn) :: !at | None -> ());
  let body = match value with Some v -> [ Text v ] | None -> [] in
  el "textarea" ?id ?style ~attrs:!at body

let select ?id ?name ?value ?style ?on_change kids =
  let at = ref [] in
  (match name with Some n -> at := Attr ("name", n) :: !at | None -> ());
  (match value with Some v -> at := Attr ("value", v) :: !at | None -> ());
  (match on_change with Some fn -> at := Handler (On_change fn) :: !at | None -> ());
  el "select" ?id ?style ~attrs:!at kids

let option ~value ?(selected = false) ?(disabled = false) label_txt =
  el "option"
    ~attrs:[ Attr ("value", value); Bool_attr ("selected", selected); Bool_attr ("disabled", disabled) ]
    [ Text label_txt ]

let checkbox ?id ?(checked = false) ?(disabled = false) ?label ?style ?on_change () =
  let box_at = ref [ Attr ("type", "checkbox"); Bool_attr ("checked", checked); Bool_attr ("disabled", disabled) ] in
  (match on_change with Some fn -> box_at := Handler (On_check fn) :: !box_at | None -> ());
  let box = el "input" ?id ~attrs:!box_at [] in
  match label with
  | None -> box
  | Some txt ->
    el "label"
      ~style:(Style.[ flex_row; items_center; gap 8; cursor_pointer ] @ Option.value ~default:[] style)
      [ box; span [ Text txt ] ]

let label ?for_ ?style kids =
  let at = match for_ with Some f -> [ Attr ("for", f) ] | None -> [] in
  el "label" ?style ~attrs:at kids

(* lists & tables *)
let ul ?id ?class_ ?style kids = el "ul" ?id ?class_ ?style kids
let ol ?id ?class_ ?style kids = el "ol" ?id ?class_ ?style kids
let li ?id ?class_ ?style kids = el "li" ?id ?class_ ?style kids
let table ?id ?class_ ?style kids = el "table" ?id ?class_ ?style kids
let thead ?style kids = el "thead" ?style kids
let tbody ?style kids = el "tbody" ?style kids
let tr ?style ?on_click kids =
  let at = match on_click with Some fn -> [ Handler (On_click fn) ] | None -> [] in
  el "tr" ?style ~attrs:at kids
let th ?colspan ?style kids =
  let at = match colspan with Some c -> [ Attr ("colspan", string_of_int c) ] | None -> [] in
  el "th" ?style ~attrs:at kids
let td ?colspan ?style kids =
  let at = match colspan with Some c -> [ Attr ("colspan", string_of_int c) ] | None -> [] in
  el "td" ?style ~attrs:at kids

(* media & inline SVG *)
let img ~src ~alt ?width ?height ?style () =
  let at = ref [ Attr ("src", src); Attr ("alt", alt) ] in
  (match width with Some w -> at := Attr ("width", string_of_int w) :: !at | None -> ());
  (match height with Some h -> at := Attr ("height", string_of_int h) :: !at | None -> ());
  el "img" ?style ~attrs:!at []

let svg ~view_box ?(width = "100%") ?(height = "100%") ?style kids =
  el "svg" ?style
    ~attrs:[ Attr ("viewBox", view_box); Attr ("width", width); Attr ("height", height); Attr ("xmlns", "http://www.w3.org/2000/svg") ]
    kids

let svg_path ~d ?(fill = "none") ?(stroke = "currentColor") ?(stroke_width = 2.0) () =
  el "path"
    ~attrs:[
      Attr ("d", d);
      Attr ("fill", fill);
      Attr ("stroke", stroke);
      Attr ("stroke-width", Printf.sprintf "%.2f" stroke_width);
      Attr ("stroke-linecap", "round");
      Attr ("stroke-linejoin", "round");
    ]
    []

let svg_rect ~x ~y ~width ~height ?(rx = 0.0) ?(fill = "currentColor") () =
  el "rect"
    ~attrs:[
      Attr ("x", Printf.sprintf "%.2f" x);
      Attr ("y", Printf.sprintf "%.2f" y);
      Attr ("width", Printf.sprintf "%.2f" width);
      Attr ("height", Printf.sprintf "%.2f" height);
      Attr ("rx", Printf.sprintf "%.2f" rx);
      Attr ("fill", fill);
    ]
    []

let svg_circle ~cx ~cy ~r ?(fill = "currentColor") () =
  el "circle"
    ~attrs:[
      Attr ("cx", Printf.sprintf "%.2f" cx);
      Attr ("cy", Printf.sprintf "%.2f" cy);
      Attr ("r", Printf.sprintf "%.2f" r);
      Attr ("fill", fill);
    ]
    []

let svg_polyline ~points ?(fill = "none") ?(stroke = "currentColor") ?(stroke_width = 2.0) () =
  let pts_str =
    String.concat " " (List.map (fun (x, y) -> Printf.sprintf "%.2f,%.2f" x y) points)
  in
  el "polyline"
    ~attrs:[
      Attr ("points", pts_str);
      Attr ("fill", fill);
      Attr ("stroke", stroke);
      Attr ("stroke-width", Printf.sprintf "%.2f" stroke_width);
    ]
    []

(* HTML + Scoped CSS + Event Registry Compiler *)
let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let is_void_tag = function
  | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input"
  | "link" | "meta" | "param" | "source" | "track" | "wbr" -> true
  | _ -> false

type compiled_view = {
  html : string;
  css : string;
  handlers : (int, handler) Hashtbl.t;
}

let compile_node (root : node) : compiled_view =
  let html_buf = Buffer.create 4096 in
  let css_rules = ref [] in
  let handlers = Hashtbl.create 64 in
  let next_id = ref 0 in
  let rec walk = function
    | Empty -> ()
    | Text s -> Buffer.add_string html_buf (escape_html s)
    | Raw_html s -> Buffer.add_string html_buf s
    | Fragment kids -> List.iter walk kids
    | Dynamic fn -> walk (fn ())
    | El { tag; id; cls; style; attrs; kids } ->
      incr next_id;
      let nid = !next_id in
      let prefix = Printf.sprintf "q%d" nid in
      let inline_css, gen_classes, extra_rules = Style.compile_styles ~cls_prefix:prefix style in
      if extra_rules <> [] then css_rules := List.rev_append extra_rules !css_rules;
      Buffer.add_char html_buf '<';
      Buffer.add_string html_buf tag;
      (match id with
       | Some i -> Printf.bprintf html_buf " id=\"%s\"" (escape_html i)
       | None -> ());
      let all_cls =
        match cls with
        | Some c when c <> "" -> c :: gen_classes
        | _ -> gen_classes
      in
      if all_cls <> [] then
        Printf.bprintf html_buf " class=\"%s\"" (escape_html (String.concat " " all_cls));
      if inline_css <> "" then
        Printf.bprintf html_buf " style=\"%s\"" (escape_html inline_css);
      List.iter
        (function
          | Attr (k, v) -> Printf.bprintf html_buf " %s=\"%s\"" k (escape_html v)
          | Bool_attr (k, true) -> Printf.bprintf html_buf " %s" k
          | Bool_attr (_, false) -> ()
          | Handler h ->
            incr next_id;
            let hid = !next_id in
            Hashtbl.replace handlers hid h;
            let attr_name =
              match h with
              | On_click _ -> "data-q-click"
              | On_input _ -> "data-q-input"
              | On_change _ -> "data-q-change"
              | On_check _ -> "data-q-check"
              | On_keydown _ -> "data-q-keydown"
              | On_submit _ -> "data-q-submit"
            in
            Printf.bprintf html_buf " %s=\"%d\"" attr_name hid)
        attrs;
      Buffer.add_char html_buf '>';
      if not (is_void_tag tag) then (
        List.iter walk kids;
        Printf.bprintf html_buf "</%s>" tag
      )
  in
  walk root;
  {
    html = Buffer.contents html_buf;
    css = String.concat "\n" (List.rev !css_rules);
    handlers;
  }

let render_to_string (root : node) : string =
  let c = compile_node root in
  if c.css = "" then c.html
  else Printf.sprintf "<style>%s</style>%s" c.css c.html
