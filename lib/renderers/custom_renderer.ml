(* Pluggable Custom Renderer Engine for Quartz.
   Think `react-reconciler`, except written as an OCaml Functor (`Make`)
   so you can compile the exact same `Dom.node` tree into:
   1. HTML5 Canvas2D / WebGL GPU Draw Command Buffers (`Canvas2d`)
   2. GitHub-Flavored Markdown Documents (`Markdown`)
   3. Native Mobile / Game-Engine JSON Scenegraphs (`Json_scene`)
   4. Or your own custom target in ~15 lines of OCaml! *)

module type BACKEND = sig
  type out

  val empty : out
  val text : string -> out
  val raw : string -> out
  val fragment : out list -> out
  val element :
    tag:string ->
    id:string option ->
    cls:string option ->
    style:Style.t list ->
    attrs:Dom.attr list ->
    kids:out list ->
    out
end

module Make (B : BACKEND) = struct
  let rec render (node : Dom.node) : B.out =
    match node with
    | Dom.Empty -> B.empty
    | Dom.Text s -> B.text s
    | Dom.Raw_html s -> B.raw s
    | Dom.Fragment ks -> B.fragment (List.map render ks)
    | Dom.Dynamic fn -> render (fn ())
    | Dom.El { tag; id; cls; style; attrs; kids } ->
      B.element ~tag ~id ~cls ~style ~attrs ~kids:(List.map render kids)
end

(* =========================================================================
   1. Markdown Renderer (`Custom_renderer.render_markdown`)
   Turns any Quartz UI page into clean GitHub-Flavored Markdown!
   ========================================================================= *)
module Markdown_backend : BACKEND with type out = string = struct
  type out = string

  let empty = ""
  let text s = s
  let raw s = s
  let fragment xs = String.concat "" (List.filter (( <> ) "") xs)

  let element ~tag ~id:_ ~cls:_ ~style:_ ~attrs ~kids =
    let body = String.trim (String.concat " " (List.filter (( <> ) "") kids)) in
    match tag with
    | "h1" -> "\n# " ^ body ^ "\n\n"
    | "h2" -> "\n## " ^ body ^ "\n\n"
    | "h3" -> "\n### " ^ body ^ "\n\n"
    | "h4" -> "\n#### " ^ body ^ "\n\n"
    | "p" -> body ^ "\n\n"
    | "strong" -> "**" ^ body ^ "**"
    | "em" -> "*" ^ body ^ "*"
    | "code" -> "`" ^ body ^ "`"
    | "pre" -> "\n```\n" ^ body ^ "\n```\n\n"
    | "li" -> "- " ^ body ^ "\n"
    | "ul" | "ol" -> "\n" ^ String.concat "" kids ^ "\n"
    | "button" -> "[ " ^ body ^ " ]"
    | "a" ->
      let href =
        List.find_map
          (function Dom.Attr ("href", u) -> Some u | _ -> None)
          attrs
        |> Option.value ~default:"#"
      in
      Printf.sprintf "[%s](%s)" body href
    | "svg" -> "`[SVG Vector Chart]`"
    | "tr" -> "| " ^ String.concat " | " (List.filter (( <> ) "") kids) ^ " |\n"
    | "thead" ->
      let row = String.concat "" kids in
      row ^ "| --- | --- | --- |\n"
    | "div" | "section" | "main" | "header" | "nav" ->
      if body = "" then "" else body ^ "\n"
    | _ -> body
end

module Markdown_renderer = Make (Markdown_backend)
let render_markdown = Markdown_renderer.render

(* =========================================================================
   2. Canvas2D GPU Draw-Call Compiler (`Custom_renderer.render_canvas2d`)
   Uses the Flexbox bounding-box layout solver to emit deterministic
   2D GPU / Canvas draw instructions (`FILL_RECT`, `DRAW_TEXT`, `VECTOR_SVG`).
   ========================================================================= *)
type draw_cmd =
  | Clear_rect of { w : int; h : int; bg : string }
  | Fill_rect of { x : int; y : int; w : int; h : int; fill : string }
  | Draw_text of { x : int; y : int; text : string; color : string; tag : string }

let compile_canvas_cmds ?(width = 800) ?(height = 600) (root : Dom.node) : draw_cmd list =
  let boxes = Tui.compute_layout ~width:(width / 10) ~height:(height / 20) root in
  let cmds =
    List.concat_map
      (fun (b : Tui.bbox) ->
        let px = b.x * 10 and py = b.y * 20 in
        let pw = b.w * 10 and ph = b.h * 20 in
        match b.tag with
        | "button" ->
          [
            Fill_rect { x = px; y = py; w = pw; h = ph; fill = "#0ea5e9" };
            Draw_text { x = px + 6; y = py + 14; text = b.text; color = "#ffffff"; tag = "button" };
          ]
        | "svg" ->
          [
            Fill_rect { x = px; y = py; w = pw; h = ph; fill = "#0f172a" };
            Draw_text { x = px + 4; y = py + 14; text = b.text; color = "#38bdf8"; tag = "svg" };
          ]
        | tag ->
          if b.text = "" then []
          else [ Draw_text { x = px; y = py + 14; text = b.text; color = "#f8fafc"; tag } ])
      boxes
  in
  Clear_rect { w = width; h = height; bg = "#020617" } :: cmds

let render_canvas2d ?(width = 800) ?(height = 600) (root : Dom.node) : string =
  let cmds = compile_canvas_cmds ~width ~height root in
  let lines =
    List.map
      (function
        | Clear_rect { w; h; bg } ->
          Printf.sprintf "ctx.fillStyle = '%s'; ctx.fillRect(0, 0, %d, %d);" bg w h
        | Fill_rect { x; y; w; h; fill } ->
          Printf.sprintf "ctx.fillStyle = '%s'; ctx.fillRect(%d, %d, %d, %d);" fill x y w h
        | Draw_text { x; y; text; color; _ } ->
          Printf.sprintf "ctx.fillStyle = '%s'; ctx.fillText(%S, %d, %d);" color text x y)
      cmds
  in
  String.concat "\n" lines

(* =========================================================================
   3. JSON Scenegraph Renderer (`Custom_renderer.render_json_scene`)
   Serializes the live UI tree into a JSON AST for native iOS/Android/Desktop.
   ========================================================================= *)
module Json_backend : BACKEND with type out = string = struct
  type out = string

  let empty = "null"
  let text s = Printf.sprintf {|{"type":"text","value":%S}|} s
  let raw s = Printf.sprintf {|{"type":"raw","html":%S}|} s
  let fragment xs =
    let non_null = List.filter (( <> ) "null") xs in
    Printf.sprintf "[%s]" (String.concat "," non_null)

  let element ~tag ~id ~cls:_ ~style ~attrs:_ ~kids =
    let id_str = match id with Some i -> Printf.sprintf "%S" i | None -> "null" in
    let css_str = Style.kvs_to_css_body style in
    let clean_kids = List.filter (( <> ) "null") kids in
    Printf.sprintf
      {|{"type":"el","tag":%S,"id":%s,"style":%S,"children":[%s]}|}
      tag id_str css_str (String.concat "," clean_kids)
end

module Json_renderer = Make (Json_backend)
let render_json_scene = Json_renderer.render
