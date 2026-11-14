(* Declarative HTML5, Inline SVG, Shadow DOM, Islands, Live 3D Canvas,
   SEO/Meta Tags, Keyed DOM Tree Diffing, and WCAG/APCA/A11y Diagnostics. *)

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
  | On_mousemove of ((int * int) -> unit)
  | On_submit of ((string * string) list -> unit)

type attr =
  | Attr of string * string
  | Bool_attr of string * bool
  | Handler of handler

type node =
  | Empty
  | Text of string
  | Raw_html of string
  | Raw_unsafe of string
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
let on_mousemove fn = Handler (On_mousemove fn)
let on_submit fn = Handler (On_submit fn)

let bind_input (sig_v : string Signal.t) =
  [ Attr ("value", Signal.get sig_v); Handler (On_input (fun s -> Signal.set sig_v s)) ]

let bind_bool (sig_b : bool Signal.t) =
  [ Bool_attr ("checked", Signal.get sig_b); Handler (On_check (fun b -> Signal.set sig_b b)) ]

let empty = Empty
let text s = Text s
let textf fmt = Printf.ksprintf (fun s -> Text s) fmt
let raw_html s = Raw_html s
let raw_unsafe s = Raw_unsafe s
let fragment kids = Fragment kids
let rx fn = Dynamic fn
let rx_text fn = Dynamic (fun () -> Text (fn ()))

let show ~when_ ?(fallback = fun () -> Empty) render =
  Dynamic (fun () -> if when_ () then render () else fallback ())

let each ~items ~key:_ render_item =
  Dynamic (fun () -> Fragment (List.mapi (fun i x -> render_item x i) (items ())))

let el tag ?id ?class_ ?(style = []) ?(attrs = []) kids =
  El { tag; id; cls = class_; style; attrs; kids }

let shadow_root ?(mode = "open") ?(scoped_css = "") kids =
  let style_node =
    if scoped_css = "" then Empty
    else el "style" [ Raw_html scoped_css ]
  in
  el "template" ~attrs:[ Attr ("shadowrootmode", mode) ] (style_node :: kids)

let custom_element tag ?(props = []) ?(style = []) ?shadow kids =
  let prop_attrs = List.map (fun (k, v) -> Attr (k, v)) props in
  let body = match shadow with Some s -> s :: kids | None -> kids in
  el tag ~style ~attrs:prop_attrs body

let view_transition ~name ?(style = []) kids =
  el "div" ~style:(Style.raw_css "view-transition-name" name :: style) kids

(* Partial Hydration Islands Architecture (`[data-q-island]`) *)
let island ~name ?(props = []) ?(style = []) inner =
  let props_json = Json.to_string (Json.obj props) in
  el "div"
    ~style
    ~attrs:[ Attr ("data-q-island", name); Attr ("data-q-props", props_json) ]
    [ inner ]

(* Embedded Live 3D Scene Canvas (`[data-q-scene3d]`) *)
let canvas3d ?(width = 640) ?(height = 360) ?(style = []) (scene : Scene3d.scene) =
  let spec = Scene3d.to_json_string scene in
  el "canvas"
    ~style:(Style.[ w_full; rounded_xl; border ~width:1 ~color:Style.Palette.slate_800 () ] @ style)
    ~attrs:
      [
        Attr ("width", string_of_int width);
        Attr ("height", string_of_int height);
        Attr ("data-q-scene3d", spec);
      ]
    []

(* Zero-cost JS Ecosystem FFI Bridge widget mount (`Quartz.Js_bridge` client runner) *)
let external_widget ~global ?(props = []) ?(style = []) fallback_kids =
  let props_json = Json.to_string (Json.obj props) in
  el "div"
    ~style
    ~attrs:[ Attr ("data-q-external", global); Attr ("data-q-props", props_json) ]
    fallback_kids

let suspense ~(resource : 'a Resource.t) ?fallback (render_ready : 'a -> node) =
  let default_skeleton () =
    el "div"
      ~style:
        Style.[
          h_px 44;
          w_full;
          rounded_lg;
          bg Palette.slate_800;
          animate_pulse;
          flex_row;
          items_center;
          px 14;
          color Palette.slate_400;
          text_xs;
        ]
      [ Text ("Loading " ^ resource.key ^ "...") ]
  in
  let fb = Option.value ~default:default_skeleton fallback in
  Dynamic
    (fun () ->
      match Resource.status resource with
      | Resource.Loading -> fb ()
      | Resource.Ready data -> render_ready data
      | Resource.Boom err ->
        el "div"
          ~style:
            Style.[
              p 12;
              rounded_lg;
              bg (`Rgba (244, 63, 94, 0.15));
              border ~width:1 ~color:Palette.rose_500 ();
              color Palette.rose_400;
              text_xs;
            ]
          [ Text ("Resource error (" ^ resource.key ^ "): " ^ err) ])

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
let button ?id ?class_ ?(style = []) ?(disabled = false) ?(type_ = "button") ?(attrs = []) ?on_click ?label kids =
  let a = ref (Attr ("type", type_) :: Bool_attr ("disabled", disabled) :: attrs) in
  (match on_click with Some fn -> a := Handler (On_click fn) :: !a | None -> ());
  let children =
    match label with
    | Some s -> Text s :: kids
    | None -> kids
  in
  el "button" ?id ?class_ ~style ~attrs:!a children

let a ~href ?(target = "_self") ?(prefetch = true) ?id ?class_ ?style ?(attrs = []) ?on_click kids =
  let at = ref (Attr ("href", href) :: Attr ("target", target) :: attrs) in
  if prefetch && String.length href > 0 && href.[0] = '/' then
    at := Attr ("data-q-prefetch", "1") :: !at;
  (match on_click with Some fn -> at := Handler (On_click fn) :: !at | None -> ());
  el "a" ?id ?class_ ?style ~attrs:!at kids

let form ?id ?class_ ?method_ ?action ?style ?(attrs = []) ?on_submit kids =
  let at = ref attrs in
  (match method_ with Some m -> at := Attr ("method", m) :: !at | None -> ());
  (match action with Some a -> at := Attr ("action", a) :: !at | None -> ());
  (match on_submit with Some fn -> at := Handler (On_submit fn) :: !at | None -> ());
  el "form" ?id ?class_ ?style ~attrs:!at kids

let input
    ?id
    ?name
    ?(type_ = "text")
    ?value
    ?placeholder
    ?min
    ?max
    ?step
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
  (match min with Some m -> at := Attr ("min", m) :: !at | None -> ());
  (match max with Some m -> at := Attr ("max", m) :: !at | None -> ());
  (match step with Some s -> at := Attr ("step", s) :: !at | None -> ());
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
let ul ?id ?class_ ?style ?attrs kids = el "ul" ?id ?class_ ?style ?attrs kids
let ol ?id ?class_ ?style ?attrs kids = el "ol" ?id ?class_ ?style ?attrs kids
let li ?id ?class_ ?style ?attrs kids = el "li" ?id ?class_ ?style ?attrs kids
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
let img ~src ~alt ?width ?height ?(loading = "lazy") ?(decoding = "async") ?style () =
  let at = ref [ Attr ("src", src); Attr ("alt", alt); Attr ("loading", loading); Attr ("decoding", decoding) ] in
  (match width with Some w -> at := Attr ("width", string_of_int w) :: !at | None -> ());
  (match height with Some h -> at := Attr ("height", string_of_int h) :: !at | None -> ());
  el "img" ?style ~attrs:!at []

let svg ~view_box ?(width = "100%") ?(height = "100%") ?style kids =
  el "svg" ?style
    ~attrs:[ Attr ("viewBox", view_box); Attr ("width", width); Attr ("height", height); Attr ("xmlns", "http://www.w3.org/2000/svg") ]
    kids

let svg_defs kids = el "defs" kids

let svg_linear_gradient ~id ?(x1 = "0%") ?(y1 = "0%") ?(x2 = "0%") ?(y2 = "100%") stops =
  el "linearGradient"
    ~attrs:[ Attr ("id", id); Attr ("x1", x1); Attr ("y1", y1); Attr ("x2", x2); Attr ("y2", y2) ]
    stops

let svg_stop ~offset ~color ?(opacity = 1.0) () =
  el "stop"
    ~attrs:[
      Attr ("offset", offset);
      Attr ("stop-color", color);
      Attr ("stop-opacity", Printf.sprintf "%.2f" opacity);
    ]
    []

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

let svg_rect ~x ~y ~width ~height ?(rx = 0.0) ?(fill = "currentColor") ?stroke () =
  let at =
    [
      Attr ("x", Printf.sprintf "%.2f" x);
      Attr ("y", Printf.sprintf "%.2f" y);
      Attr ("width", Printf.sprintf "%.2f" width);
      Attr ("height", Printf.sprintf "%.2f" height);
      Attr ("rx", Printf.sprintf "%.2f" rx);
      Attr ("fill", fill);
    ]
  in
  let at = match stroke with Some s -> Attr ("stroke", s) :: at | None -> at in
  el "rect" ~attrs:at []

let svg_circle ~cx ~cy ~r ?(fill = "currentColor") ?stroke ?stroke_width ?stroke_dasharray ?stroke_dashoffset () =
  let at =
    ref
      [
        Attr ("cx", Printf.sprintf "%.2f" cx);
        Attr ("cy", Printf.sprintf "%.2f" cy);
        Attr ("r", Printf.sprintf "%.2f" r);
        Attr ("fill", fill);
      ]
  in
  (match stroke with Some s -> at := Attr ("stroke", s) :: !at | None -> ());
  (match stroke_width with Some sw -> at := Attr ("stroke-width", Printf.sprintf "%.2f" sw) :: !at | None -> ());
  (match stroke_dasharray with Some da -> at := Attr ("stroke-dasharray", da) :: !at | None -> ());
  (match stroke_dashoffset with Some doff -> at := Attr ("stroke-dashoffset", Printf.sprintf "%.2f" doff) :: !at | None -> ());
  el "circle" ~attrs:!at []

let svg_line ~x1 ~y1 ~x2 ~y2 ?(stroke = "currentColor") ?(stroke_width = 1.0) ?stroke_dasharray () =
  let at =
    ref
      [
        Attr ("x1", Printf.sprintf "%.2f" x1);
        Attr ("y1", Printf.sprintf "%.2f" y1);
        Attr ("x2", Printf.sprintf "%.2f" x2);
        Attr ("y2", Printf.sprintf "%.2f" y2);
        Attr ("stroke", stroke);
        Attr ("stroke-width", Printf.sprintf "%.2f" stroke_width);
      ]
  in
  (match stroke_dasharray with Some d -> at := Attr ("stroke-dasharray", d) :: !at | None -> ());
  el "line" ~attrs:!at []

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
      Attr ("stroke-linecap", "round");
      Attr ("stroke-linejoin", "round");
    ]
    []

let svg_polygon ~points ?(fill = "currentColor") ?stroke ?(stroke_width = 1.5) () =
  let pts_str =
    String.concat " " (List.map (fun (x, y) -> Printf.sprintf "%.2f,%.2f" x y) points)
  in
  let at = ref [ Attr ("points", pts_str); Attr ("fill", fill) ] in
  (match stroke with
   | Some s ->
     at := Attr ("stroke", s) :: Attr ("stroke-width", Printf.sprintf "%.2f" stroke_width) :: !at
   | None -> ());
  el "polygon" ~attrs:!at []

let svg_text ~x ~y ?(fill = "#94a3b8") ?(font_size = 11) ?(anchor = "start") txt =
  el "text"
    ~attrs:[
      Attr ("x", Printf.sprintf "%.2f" x);
      Attr ("y", Printf.sprintf "%.2f" y);
      Attr ("fill", fill);
      Attr ("font-size", string_of_int font_size);
      Attr ("text-anchor", anchor);
      Attr ("font-family", "Inter, system-ui, sans-serif");
    ]
    [ Text txt ]

(* SEO / Open Graph / JSON-LD Page Metadata *)
type page_meta = {
  title : string;
  description : string option;
  canonical : string option;
  og_image : string option;
  og_type : string;
  lang : string;
  json_ld : string option;
}

let default_meta ?(title = "Quartz App") () =
  { title; description = None; canonical = None; og_image = None; og_type = "website"; lang = "en"; json_ld = None }

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

let render_meta_tags (m : page_meta) : string =
  let lines = ref [] in
  let add l = lines := l :: !lines in
  (match m.description with
   | Some d ->
     add (Printf.sprintf {|  <meta name="description" content="%s" />|} (escape_html d));
     add (Printf.sprintf {|  <meta property="og:description" content="%s" />|} (escape_html d))
   | None -> ());
  add (Printf.sprintf {|  <meta property="og:title" content="%s" />|} (escape_html m.title));
  add (Printf.sprintf {|  <meta property="og:type" content="%s" />|} (escape_html m.og_type));
  (match m.canonical with
   | Some c ->
     add (Printf.sprintf {|  <link rel="canonical" href="%s" />|} (escape_html c));
     add (Printf.sprintf {|  <meta property="og:url" content="%s" />|} (escape_html c))
   | None -> ());
  (match m.og_image with
   | Some img ->
     add (Printf.sprintf {|  <meta property="og:image" content="%s" />|} (escape_html img));
     add {|  <meta name="twitter:card" content="summary_large_image" />|}
   | None -> ());
  (match m.json_ld with
   | Some ld ->
     add (Printf.sprintf {|  <script type="application/ld+json">%s</script>|} ld)
   | None -> ());
  String.concat "\n" (List.rev !lines)

let is_void_tag = function
  | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input"
  | "link" | "meta" | "param" | "source" | "track" | "wbr" -> true
  | _ -> false

(* Materialized Snapshot Tree (for Keyed Virtual/SSR DOM Diffing) *)
type vnode =
  | V_empty
  | V_text of string
  | V_el of {
      tag : string;
      attrs : (string * string) list;
      kids : vnode list;
      raw_html : string;
    }

type compiled_view = {
  html : string;
  css : string;
  handlers : (int, handler) Hashtbl.t;
  snapshot : vnode;
}

let compile_node (root : node) : compiled_view =
  let html_buf = Buffer.create 4096 in
  let css_rules = ref [] in
  let seen_css = Hashtbl.create 32 in
  let handlers = Hashtbl.create 64 in
  let next_id = ref 0 in
  let add_rule r =
    if not (Hashtbl.mem seen_css r) then (
      Hashtbl.add seen_css r ();
      css_rules := r :: !css_rules
    )
  in
  let rec walk_to_buf buf n =
    match n with
    | Empty -> V_empty
    | Text s ->
      Buffer.add_string buf (escape_html s);
      V_text s
    | Raw_html s | Raw_unsafe s ->
      Buffer.add_string buf s;
      V_text s
    | Fragment ks ->
      let vk = List.filter_map (fun k -> match walk_to_buf buf k with V_empty -> None | v -> Some v) ks in
      (match vk with
       | [ single ] -> single
       | many -> V_el { tag = "fragment"; attrs = []; kids = many; raw_html = "" })
    | Dynamic fn -> walk_to_buf buf (fn ())
    | El { tag; id; cls; style; attrs; kids } ->
      incr next_id;
      let nid = !next_id in
      let prefix = Printf.sprintf "q%d" nid in
      let inline_css, gen_classes, extra_rules = Style.compile_styles ~cls_prefix:prefix style in
      List.iter add_rule extra_rules;
      let snap_attrs = ref [] in
      let elem_buf = Buffer.create 128 in
      Buffer.add_char elem_buf '<';
      Buffer.add_string elem_buf tag;
      (match id with
       | Some i ->
         snap_attrs := ("id", i) :: !snap_attrs;
         Printf.bprintf elem_buf " id=\"%s\"" (escape_html i)
       | None -> ());
      let all_cls =
        match cls with
        | Some c when c <> "" -> c :: gen_classes
        | _ -> gen_classes
      in
      if all_cls <> [] then (
        let cstr = String.concat " " all_cls in
        snap_attrs := ("class", cstr) :: !snap_attrs;
        Printf.bprintf elem_buf " class=\"%s\"" (escape_html cstr)
      );
      if inline_css <> "" then (
        snap_attrs := ("style", inline_css) :: !snap_attrs;
        Printf.bprintf elem_buf " style=\"%s\"" (escape_html inline_css)
      );
      List.iter
        (function
          | Attr (k, v) ->
            snap_attrs := (k, v) :: !snap_attrs;
            Printf.bprintf elem_buf " %s=\"%s\"" k (escape_html v)
          | Bool_attr (k, true) ->
            snap_attrs := (k, "") :: !snap_attrs;
            Printf.bprintf elem_buf " %s" k
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
              | On_mousemove _ -> "data-q-mousemove"
              | On_submit _ -> "data-q-submit"
            in
            let hids = string_of_int hid in
            snap_attrs := (attr_name, hids) :: !snap_attrs;
            Printf.bprintf elem_buf " %s=\"%s\"" attr_name hids)
        attrs;
      Buffer.add_char elem_buf '>';
      let vkids =
        if is_void_tag tag then []
        else (
          let res =
            List.filter_map
              (fun k -> match walk_to_buf elem_buf k with V_empty -> None | v -> Some v)
              kids
          in
          Printf.bprintf elem_buf "</%s>" tag;
          res
        )
      in
      let el_html = Buffer.contents elem_buf in
      Buffer.add_string buf el_html;
      V_el { tag; attrs = List.rev !snap_attrs; kids = vkids; raw_html = el_html }
  in
  let snap = walk_to_buf html_buf root in
  {
    html = Buffer.contents html_buf;
    css = String.concat "\n" (Style.deduplicate_css (List.rev !css_rules));
    handlers;
    snapshot = snap;
  }

let render_to_string (root : node) : string =
  let c = compile_node root in
  if c.css = "" then c.html
  else Printf.sprintf "<style>%s</style>%s" c.css c.html

(* Keyed Virtual/SSR DOM Tree Diffing *)
type patch =
  | Set_text of { path : int list; text : string }
  | Set_attr of { path : int list; key : string; value : string }
  | Remove_attr of { path : int list; key : string }
  | Replace_node of { path : int list; html : string }
  | Append_child of { path : int list; html : string }
  | Remove_child of { path : int list; index : int }

let render_vnode_fallback = function
  | V_empty -> ""
  | V_text s -> escape_html s
  | V_el { raw_html; _ } -> raw_html

let diff (old_v : vnode) (new_v : vnode) : patch list =
  let acc = ref [] in
  let rec go path a b =
    match a, b with
    | V_empty, V_empty -> ()
    | V_text s1, V_text s2 ->
      if s1 <> s2 then acc := Set_text { path = List.rev path; text = s2 } :: !acc
    | V_el e1, V_el e2 when e1.tag = e2.tag ->
      if e1.raw_html <> e2.raw_html then (
        (* Diff attributes *)
        List.iter
          (fun (k, _) ->
            if not (List.mem_assoc k e2.attrs) then
              acc := Remove_attr { path = List.rev path; key = k } :: !acc)
          e1.attrs;
        List.iter
          (fun (k, v) ->
            match List.assoc_opt k e1.attrs with
            | Some v0 when v0 = v -> ()
            | _ -> acc := Set_attr { path = List.rev path; key = k; value = v } :: !acc)
          e2.attrs;
        (* Diff children *)
        let rec loop i ks1 ks2 =
          match ks1, ks2 with
          | [], [] -> ()
          | k1 :: r1, k2 :: r2 ->
            go (i :: path) k1 k2;
            loop (i + 1) r1 r2
          | [], k2 :: r2 ->
            acc := Append_child { path = List.rev path; html = render_vnode_fallback k2 } :: !acc;
            loop (i + 1) [] r2
          | _ :: r1, [] ->
            acc := Remove_child { path = List.rev path; index = i } :: !acc;
            loop i r1 []
        in
        loop 0 e1.kids e2.kids
      )
    | _, b_node ->
      acc := Replace_node { path = List.rev path; html = render_vnode_fallback b_node } :: !acc
  in
  go [] old_v new_v;
  List.rev !acc

let patch_to_json = function
  | Set_text { path; text } ->
    Json.obj
      [
        ("op", Json.string "set_text");
        ("path", Json.array (List.map Json.int path));
        ("text", Json.string text);
      ]
  | Set_attr { path; key; value } ->
    Json.obj
      [
        ("op", Json.string "set_attr");
        ("path", Json.array (List.map Json.int path));
        ("key", Json.string key);
        ("value", Json.string value);
      ]
  | Remove_attr { path; key } ->
    Json.obj
      [
        ("op", Json.string "remove_attr");
        ("path", Json.array (List.map Json.int path));
        ("key", Json.string key);
      ]
  | Replace_node { path; html } ->
    Json.obj
      [
        ("op", Json.string "replace");
        ("path", Json.array (List.map Json.int path));
        ("html", Json.string html);
      ]
  | Append_child { path; html } ->
    Json.obj
      [
        ("op", Json.string "append");
        ("path", Json.array (List.map Json.int path));
        ("html", Json.string html);
      ]
  | Remove_child { path; index } ->
    Json.obj
      [
        ("op", Json.string "remove_child");
        ("path", Json.array (List.map Json.int path));
        ("index", Json.int index);
      ]

let patches_to_json ps = Json.array (List.map patch_to_json ps)

(* Extended Static Analyzer (WCAG 2.1 + APCA + A11y + Islands) *)
type diag_report = {
  element_count : int;
  handler_count : int;
  scoped_css_count : int;
  internal_links : string list;
  a11y_warnings : string list;
  contrast_checks : int;
  contrast_warnings : string list;
  apca_checks : int;
  apca_warnings : string list;
  islands : string list;
}

let extract_hex_style key (styles : Style.t list) : Style.color option =
  let found = ref None in
  List.iter
    (function
      | Style.Kv (k, v) when k = key && String.length v > 0 && v.[0] = '#' ->
        found := Some (`Hex v)
      | _ -> ())
    styles;
  !found

let diagnose (root : node) : diag_report =
  let els = ref 0 in
  let hnds = ref 0 in
  let css_n = ref 0 in
  let links = ref [] in
  let warns = ref [] in
  let c_checks = ref 0 in
  let c_warns = ref [] in
  let a_checks = ref 0 in
  let a_warns = ref [] in
  let islands = ref [] in
  let rec scan inherited_bg = function
    | Empty | Text _ | Raw_html _ | Raw_unsafe _ -> ()
    | Fragment ks -> List.iter (scan inherited_bg) ks
    | Dynamic fn -> scan inherited_bg (fn ())
    | El { tag; style; attrs; kids; _ } ->
      incr els;
      let _, _, extra = Style.compile_styles ~cls_prefix:"diag" style in
      css_n := !css_n + List.length extra;
      let cur_bg =
        match extract_hex_style "background-color" style with
        | Some b -> b
        | None -> inherited_bg
      in
      (match extract_hex_style "color" style with
       | Some fg ->
         incr c_checks;
         let ratio = Contrast.contrast_ratio fg cur_bg in
         if ratio < 3.0 then
           c_warns :=
             Printf.sprintf "<%s> contrast ratio %.2f:1 is below WCAG threshold (3.0:1)" tag ratio
             :: !c_warns;
         incr a_checks;
         let lc = Contrast.apca_lc ~fg ~bg:cur_bg in
         if Float.abs lc < 35.0 then
           a_warns :=
             Printf.sprintf "<%s> APCA perceptual Lc %.1f is below threshold (35.0)" tag lc
             :: !a_warns
       | None -> ());
      let has_alt = ref false in
      let has_label = ref false in
      let is_clickable = ref false in
      let has_role = ref false in
      List.iter
        (function
          | Handler h ->
            incr hnds;
            (match h with On_click _ -> is_clickable := true | _ -> ())
          | Attr ("data-q-island", name) ->
            if not (List.mem name !islands) then islands := name :: !islands
          | Attr ("href", u) when String.length u > 0 && u.[0] = '/' ->
            if not (List.mem u !links) then links := u :: !links
          | Attr ("alt", a) when String.trim a <> "" -> has_alt := true
          | Attr (("aria-label" | "aria-labelledby" | "placeholder" | "title"), v) when String.trim v <> "" ->
            has_label := true
          | Attr ("role", _) -> has_role := true
          | _ -> ())
        attrs;
      if tag = "img" && not !has_alt then
        warns := "<img> element is missing a non-empty alt attribute" :: !warns;
      if tag = "button" && kids = [] && not !has_label then
        warns := "<button> element has no label or child nodes" :: !warns;
      if tag = "input" && not !has_label then (
        let is_hidden_or_hp =
          List.exists (function Attr ("type", ("hidden" | "checkbox" | "range")) -> true | _ -> false) attrs
        in
        if not is_hidden_or_hp then
          warns := "<input> element is missing placeholder or aria-label" :: !warns
      );
      if tag = "div" && !is_clickable && not !has_role then
        warns := "clickable <div> missing role attribute (prefer <button>)" :: !warns;
      List.iter (scan cur_bg) kids
  in
  scan Style.Palette.slate_950 root;
  {
    element_count = !els;
    handler_count = !hnds;
    scoped_css_count = !css_n;
    internal_links = List.rev !links;
    a11y_warnings = List.rev !warns;
    contrast_checks = !c_checks;
    contrast_warnings = List.rev !c_warns;
    apca_checks = !a_checks;
    apca_warnings = List.rev !a_warns;
    islands = List.rev !islands;
  }
