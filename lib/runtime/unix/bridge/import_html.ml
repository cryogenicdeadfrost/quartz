(* HTML + Tailwind Utility -> `Quartz.Dom` / `Quartz.Style` Codegen CLI (`quartz import-html`).
   Converts copy-pasted HTML / Tailwind / shadcn-like markup snippets into clean
   OCaml Quartz expressions. *)

type hnode =
  | HText of string
  | HEl of { tag : string; attrs : (string * string) list; kids : hnode list }

let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let parse_attrs raw =
  let len = String.length raw in
  let rec skip i = if i < len && is_ws raw.[i] then skip (i + 1) else i in
  let read_ident i =
    let start = i in
    let j = ref i in
    while !j < len && (not (is_ws raw.[!j])) && raw.[!j] <> '=' && raw.[!j] <> '>' && raw.[!j] <> '/' do
      incr j
    done;
    (String.sub raw start (!j - start), !j)
  in
  let rec loop i acc =
    let i = skip i in
    if i >= len || raw.[i] = '>' || raw.[i] = '/' then List.rev acc
    else
      let k, after_k = read_ident i in
      if k = "" then loop (i + 1) acc
      else
        let j = skip after_k in
        if j < len && raw.[j] = '=' then
          let vstart = skip (j + 1) in
          if vstart < len && (raw.[vstart] = '"' || raw.[vstart] = '\'') then
            let q = raw.[vstart] in
            let vend = ref (vstart + 1) in
            while !vend < len && raw.[!vend] <> q do incr vend done;
            let v = String.sub raw (vstart + 1) (!vend - vstart - 1) in
            loop (min len (!vend + 1)) ((k, v) :: acc)
          else
            let v, after_v = read_ident vstart in
            loop after_v ((k, v) :: acc)
        else loop j ((k, "") :: acc)
  in
  loop 0 []

let is_self_closing = function
  | "img" | "input" | "br" | "hr" | "meta" | "link" -> true
  | _ -> false

let parse_html (src : string) : hnode list =
  let len = String.length src in
  let pos = ref 0 in
  let rec parse_children stop_tag =
    let acc = ref [] in
    let cont = ref true in
    while !cont && !pos < len do
      if src.[!pos] = '<' then (
        if !pos + 1 < len && src.[!pos + 1] = '/' then (
          let end_c = match String.index_from_opt src !pos '>' with Some i -> i + 1 | None -> len in
          pos := end_c;
          cont := false
        ) else if !pos + 3 < len && String.sub src !pos 4 = "<!--" then (
          let end_c =
            let rec f i =
              if i + 2 >= len then len
              else if String.sub src i 3 = "-->" then i + 3
              else f (i + 1)
            in
            f (!pos + 4)
          in
          pos := end_c
        ) else (
          match String.index_from_opt src !pos '>' with
          | None -> cont := false
          | Some close_gt ->
            let self_close = close_gt > !pos && src.[close_gt - 1] = '/' in
            let inner = String.trim (String.sub src (!pos + 1) (close_gt - !pos - 1 - (if self_close then 1 else 0))) in
            pos := close_gt + 1;
            let sp = ref 0 in
            while !sp < String.length inner && not (is_ws inner.[!sp]) do incr sp done;
            let tag = String.lowercase_ascii (String.sub inner 0 !sp) in
            let rest_attr = String.sub inner !sp (String.length inner - !sp) in
            let attrs = parse_attrs rest_attr in
            let kids =
              if self_close || is_self_closing tag then []
              else parse_children (Some tag)
            in
            acc := HEl { tag; attrs; kids } :: !acc
        )
      ) else (
        let next_lt = match String.index_from_opt src !pos '<' with Some i -> i | None -> len in
        let txt = String.trim (String.sub src !pos (next_lt - !pos)) in
        pos := next_lt;
        if txt <> "" then acc := HText txt :: !acc
      )
    done;
    ignore stop_tag;
    List.rev !acc
  in
  parse_children None

let map_tailwind_class = function
  | "flex" -> Some "flex"
  | "flex-row" -> Some "flex_row"
  | "flex-col" -> Some "flex_col"
  | "flex-wrap" -> Some "flex_wrap"
  | "items-center" -> Some "items_center"
  | "justify-between" -> Some "justify_between"
  | "justify-center" -> Some "justify_center"
  | "w-full" -> Some "w_full"
  | "h-full" -> Some "h_full"
  | "rounded-md" -> Some "rounded_md"
  | "rounded-lg" -> Some "rounded_lg"
  | "rounded-xl" -> Some "rounded_xl"
  | "rounded-full" -> Some "rounded_full"
  | "font-bold" | "bold" -> Some "bold"
  | "font-semibold" -> Some "font_semibold"
  | "text-xs" -> Some "text_xs"
  | "text-sm" -> Some "text_sm"
  | "text-base" -> Some "text_base"
  | "text-lg" -> Some "text_lg"
  | "text-xl" -> Some "text_xl"
  | "text-2xl" -> Some "text_2xl"
  | "text-3xl" -> Some "text_3xl"
  | "text-white" -> Some "color Palette.white"
  | "text-slate-300" -> Some "color Palette.slate_300"
  | "text-slate-400" -> Some "color Palette.slate_400"
  | "text-sky-400" -> Some "color Palette.sky_400"
  | "bg-slate-950" -> Some "bg Palette.slate_950"
  | "bg-slate-900" -> Some "bg Palette.slate_900"
  | "bg-slate-800" -> Some "bg Palette.slate_800"
  | "bg-sky-500" -> Some "bg Palette.sky_500"
  | "shadow-md" -> Some "shadow_md"
  | "shadow-lg" -> Some "shadow_lg"
  | "cursor-pointer" -> Some "cursor_pointer"
  | "border" -> Some "border ()"
  | cls when String.length cls > 2 && String.sub cls 0 2 = "p-" ->
    Option.map (fun n -> Printf.sprintf "p %d" (n * 4))
      (int_of_string_opt (String.sub cls 2 (String.length cls - 2)))
  | cls when String.length cls > 3 && String.sub cls 0 3 = "px-" ->
    Option.map (fun n -> Printf.sprintf "px %d" (n * 4))
      (int_of_string_opt (String.sub cls 3 (String.length cls - 3)))
  | cls when String.length cls > 3 && String.sub cls 0 3 = "py-" ->
    Option.map (fun n -> Printf.sprintf "py %d" (n * 4))
      (int_of_string_opt (String.sub cls 3 (String.length cls - 3)))
  | cls when String.length cls > 4 && String.sub cls 0 4 = "gap-" ->
    Option.map (fun n -> Printf.sprintf "gap %d" (n * 4))
      (int_of_string_opt (String.sub cls 4 (String.length cls - 4)))
  | cls when String.length cls > 10 && String.sub cls 0 10 = "grid-cols-" ->
    Option.map (fun n -> Printf.sprintf "grid_cols %d" n)
      (int_of_string_opt (String.sub cls 10 (String.length cls - 10)))
  | _ -> None

let tag_helper = function
  | "div" | "span" | "main" | "section" | "article" | "header" | "footer"
  | "nav" | "aside" | "h1" | "h2" | "h3" | "h4" | "p" | "code" | "pre"
  | "strong" | "em" | "ul" | "ol" | "li" | "table" | "thead" | "tbody"
  | "tr" | "th" | "td" | "button" | "form" | "label" as t -> "Dom." ^ t
  | t -> Printf.sprintf "Dom.el %S" t

let rec to_ocaml ?(indent = 0) node : string =
  let pad = String.make indent ' ' in
  match node with
  | HText s -> Printf.sprintf "%sDom.text %S" pad s
  | HEl { tag = "a"; attrs; kids } ->
    let href = Option.value ~default:"/" (List.assoc_opt "href" attrs) in
    let cls = Option.value ~default:"" (List.assoc_opt "class" attrs) in
    let styles =
      String.split_on_char ' ' cls |> List.filter_map map_tailwind_class
    in
    let style_arg =
      if styles = [] then "" else Printf.sprintf " ~style:Style.[ %s ]" (String.concat "; " styles)
    in
    let kids_s =
      if kids = [] then "[]"
      else "[\n" ^ String.concat ";\n" (List.map (to_ocaml ~indent:(indent + 2)) kids) ^ "\n" ^ pad ^ "]"
    in
    Printf.sprintf "%sDom.a ~href:%S%s %s" pad href style_arg kids_s
  | HEl { tag = "img"; attrs; _ } ->
    let src = Option.value ~default:"/image.png" (List.assoc_opt "src" attrs) in
    let alt = Option.value ~default:"Image" (List.assoc_opt "alt" attrs) in
    let w = Option.bind (List.assoc_opt "width" attrs) int_of_string_opt |> Option.value ~default:320 in
    let h = Option.bind (List.assoc_opt "height" attrs) int_of_string_opt |> Option.value ~default:180 in
    Printf.sprintf "%sDom.img ~src:%S ~alt:%S ~width:%d ~height:%d ()" pad src alt w h
  | HEl { tag = "dialog"; attrs; kids } ->
    let did = Option.value ~default:"modal-dialog" (List.assoc_opt "id" attrs) in
    let is_open = List.mem_assoc "open" attrs in
    let kids_s =
      if kids = [] then "[]"
      else "[\n" ^ String.concat ";\n" (List.map (to_ocaml ~indent:(indent + 2)) kids) ^ "\n" ^ pad ^ "]"
    in
    Printf.sprintf "%sHeadless.dialog ~id:%S ~open_:%b %s" pad did is_open kids_s
  | HEl { tag = "details"; attrs; kids } ->
    let is_open = List.mem_assoc "open" attrs in
    let sum_kids, body_kids =
      match kids with
      | HEl { tag = "summary"; kids = sk; _ } :: rest -> (sk, rest)
      | rest -> ([ HText "Details" ], rest)
    in
    let sk_s = String.concat "; " (List.map (to_ocaml ~indent:0) sum_kids) in
    let bk_s =
      if body_kids = [] then "[]"
      else "[\n" ^ String.concat ";\n" (List.map (to_ocaml ~indent:(indent + 2)) body_kids) ^ "\n" ^ pad ^ "]"
    in
    Printf.sprintf "%sHeadless.accordion_item ~open_:%b ~summary_kids:[ %s ] %s" pad is_open sk_s bk_s
  | HEl { tag; attrs; kids } ->
    let cls = Option.value ~default:"" (List.assoc_opt "class" attrs) in
    let styles =
      String.split_on_char ' ' cls |> List.filter_map map_tailwind_class
    in
    let style_arg =
      if styles = [] then "" else Printf.sprintf " ~style:Style.[ %s ]" (String.concat "; " styles)
    in
    let extra_attrs =
      List.filter (fun (k, _) -> k <> "class" && k <> "id") attrs
    in
    let attr_arg =
      if extra_attrs = [] then ""
      else
        " ~attrs:[ "
        ^ String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "Dom.attr %S %S" k v) extra_attrs)
        ^ " ]"
    in
    let kids_s =
      if kids = [] then "[]"
      else "[\n" ^ String.concat ";\n" (List.map (to_ocaml ~indent:(indent + 2)) kids) ^ "\n" ^ pad ^ "]"
    in
    Printf.sprintf "%s%s%s%s %s" pad (tag_helper tag) style_arg attr_arg kids_s

let convert_html_to_ocaml (html_src : string) : string =
  let nodes = parse_html html_src in
  match nodes with
  | [] -> "Dom.empty"
  | [ single ] -> to_ocaml ~indent:0 single
  | many ->
    "Dom.fragment [\n" ^ String.concat ";\n" (List.map (to_ocaml ~indent:2) many) ^ "\n]"
