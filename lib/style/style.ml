type color =
  [ `Hex of string
  | `Rgb of int * int * int
  | `Rgba of int * int * int * float
  | `Var of string ]

let color_to_css : color -> string = function
  | `Hex h -> if String.length h > 0 && h.[0] = '#' then h else "#" ^ h
  | `Rgb (r, g, b) -> Printf.sprintf "rgb(%d,%d,%d)" r g b
  | `Rgba (r, g, b, a) -> Printf.sprintf "rgba(%d,%d,%d,%.2f)" r g b a
  | `Var v -> Printf.sprintf "var(%s)" v

module Palette = struct
  let white = `Hex "#ffffff"
  let black = `Hex "#000000"
  let transparent = `Rgba (0, 0, 0, 0.0)
  let slate_50 = `Hex "#f8fafc"
  let slate_100 = `Hex "#f1f5f9"
  let slate_200 = `Hex "#e2e8f0"
  let slate_300 = `Hex "#cbd5e1"
  let slate_400 = `Hex "#94a3b8"
  let slate_500 = `Hex "#64748b"
  let slate_600 = `Hex "#475569"
  let slate_700 = `Hex "#334155"
  let slate_800 = `Hex "#1e293b"
  let slate_900 = `Hex "#0f172a"
  let slate_950 = `Hex "#020617"

  let sky_400 = `Hex "#38bdf8"
  let sky_500 = `Hex "#0ea5e9"
  let sky_600 = `Hex "#0284c7"
  let indigo_400 = `Hex "#818cf8"
  let indigo_500 = `Hex "#6366f1"
  let indigo_600 = `Hex "#4f46e5"
  let violet_500 = `Hex "#8b5cf6"
  let emerald_400 = `Hex "#34d399"
  let emerald_500 = `Hex "#10b981"
  let emerald_600 = `Hex "#059669"
  let amber_400 = `Hex "#fbbf24"
  let amber_500 = `Hex "#f59e0b"
  let rose_400 = `Hex "#fb7185"
  let rose_500 = `Hex "#f43f5e"
  let rose_600 = `Hex "#e11d48"
end

type t =
  | Kv of string * string
  | Hover of t list
  | Focus of t list
  | Active of t list
  | Dark of t list
  | Media of int * t list

let raw_css k v = Kv (k, v)
let css_var name value = Kv (name, value)

(* display & flex *)
let block = Kv ("display", "block")
let inline_block = Kv ("display", "inline-block")
let inline = Kv ("display", "inline")
let hidden = Kv ("display", "none")
let flex = Kv ("display", "flex")
let flex_row = Kv ("display", "flex;flex-direction:row")
let flex_col = Kv ("display", "flex;flex-direction:column")
let flex_wrap = Kv ("flex-wrap", "wrap")
let flex_nowrap = Kv ("flex-wrap", "nowrap")
let flex_1 = Kv ("flex", "1 1 0%")
let flex_grow n = Kv ("flex-grow", string_of_int n)
let flex_shrink n = Kv ("flex-shrink", string_of_int n)

let grid = Kv ("display", "grid")
let grid_cols n =
  Kv ("display", Printf.sprintf "grid;grid-template-columns:repeat(%d,minmax(0,1fr))" n)
let grid_rows n =
  Kv ("grid-template-rows", Printf.sprintf "repeat(%d,minmax(0,1fr))" n)
let col_span n = Kv ("grid-column", Printf.sprintf "span %d / span %d" n n)
let row_span n = Kv ("grid-row", Printf.sprintf "span %d / span %d" n n)

let gap px = Kv ("gap", Printf.sprintf "%dpx" px)
let gap_x px = Kv ("column-gap", Printf.sprintf "%dpx" px)
let gap_y px = Kv ("row-gap", Printf.sprintf "%dpx" px)

let justify_start = Kv ("justify-content", "flex-start")
let justify_center = Kv ("justify-content", "center")
let justify_end = Kv ("justify-content", "flex-end")
let justify_between = Kv ("justify-content", "space-between")
let justify_around = Kv ("justify-content", "space-around")
let justify_evenly = Kv ("justify-content", "space-evenly")

let items_start = Kv ("align-items", "flex-start")
let items_center = Kv ("align-items", "center")
let items_end = Kv ("align-items", "flex-end")
let items_baseline = Kv ("align-items", "baseline")
let items_stretch = Kv ("align-items", "stretch")

let align_center = items_center
let align_start = items_start
let align_end = items_end

(* box sizing & spacing *)
let w_px n = Kv ("width", Printf.sprintf "%dpx" n)
let h_px n = Kv ("height", Printf.sprintf "%dpx" n)
let w_pct p = Kv ("width", Printf.sprintf "%.1f%%" p)
let h_pct p = Kv ("height", Printf.sprintf "%.1f%%" p)
let w_full = Kv ("width", "100%")
let h_full = Kv ("height", "100%")
let w_screen = Kv ("width", "100vw")
let h_screen = Kv ("min-height", "100vh")
let min_w n = Kv ("min-width", Printf.sprintf "%dpx" n)
let max_w n = Kv ("max-width", Printf.sprintf "%dpx" n)
let min_h n = Kv ("min-height", Printf.sprintf "%dpx" n)
let max_h n = Kv ("max-height", Printf.sprintf "%dpx" n)

let p n = Kv ("padding", Printf.sprintf "%dpx" n)
let px n = Kv ("padding-left", Printf.sprintf "%dpx;padding-right:%dpx" n n)
let py n = Kv ("padding-top", Printf.sprintf "%dpx;padding-bottom:%dpx" n n)
let pt n = Kv ("padding-top", Printf.sprintf "%dpx" n)
let pr n = Kv ("padding-right", Printf.sprintf "%dpx" n)
let pb n = Kv ("padding-bottom", Printf.sprintf "%dpx" n)
let pl n = Kv ("padding-left", Printf.sprintf "%dpx" n)
let padding = p

let m n = Kv ("margin", Printf.sprintf "%dpx" n)
let mx n = Kv ("margin-left", Printf.sprintf "%dpx;margin-right:%dpx" n n)
let my n = Kv ("margin-top", Printf.sprintf "%dpx;margin-bottom:%dpx" n n)
let mt n = Kv ("margin-top", Printf.sprintf "%dpx" n)
let mr n = Kv ("margin-right", Printf.sprintf "%dpx" n)
let mb n = Kv ("margin-bottom", Printf.sprintf "%dpx" n)
let ml n = Kv ("margin-left", Printf.sprintf "%dpx" n)
let mx_auto = Kv ("margin-left", "auto;margin-right:auto")

let relative = Kv ("position", "relative")
let absolute = Kv ("position", "absolute")
let fixed = Kv ("position", "fixed")
let sticky = Kv ("position", "sticky")
let top n = Kv ("top", Printf.sprintf "%dpx" n)
let right n = Kv ("right", Printf.sprintf "%dpx" n)
let bottom n = Kv ("bottom", Printf.sprintf "%dpx" n)
let left n = Kv ("left", Printf.sprintf "%dpx" n)
let inset_0 = Kv ("inset", "0px")
let z_index n = Kv ("z-index", string_of_int n)

let overflow_hidden = Kv ("overflow", "hidden")
let overflow_auto = Kv ("overflow", "auto")
let overflow_x_auto = Kv ("overflow-x", "auto")
let overflow_y_auto = Kv ("overflow-y", "auto")

(* colors, borders, shadows & glassmorphism *)
let color c = Kv ("color", color_to_css c)
let bg c = Kv ("background-color", color_to_css c)
let bg_gradient ~dir ~from_ ~to_ =
  let d =
    match dir with
    | `To_r -> "to right"
    | `To_br -> "to bottom right"
    | `To_b -> "to bottom"
  in
  Kv ("background-image", Printf.sprintf "linear-gradient(%s, %s, %s)" d (color_to_css from_) (color_to_css to_))

let glass =
  Kv ("background-color", "rgba(15,23,42,0.72);backdrop-filter:blur(12px);-webkit-backdrop-filter:blur(12px)")

let border ?(width = 1) ?(color = Palette.slate_700) () =
  Kv ("border", Printf.sprintf "%dpx solid %s" width (color_to_css color))
let border_b ?(width = 1) ?(color = Palette.slate_800) () =
  Kv ("border-bottom", Printf.sprintf "%dpx solid %s" width (color_to_css color))
let border_t ?(width = 1) ?(color = Palette.slate_800) () =
  Kv ("border-top", Printf.sprintf "%dpx solid %s" width (color_to_css color))

let rounded_none = Kv ("border-radius", "0px")
let rounded_sm = Kv ("border-radius", "4px")
let rounded_md = Kv ("border-radius", "6px")
let rounded_lg = Kv ("border-radius", "8px")
let rounded_xl = Kv ("border-radius", "12px")
let rounded_2xl = Kv ("border-radius", "16px")
let rounded_full = Kv ("border-radius", "9999px")
let rounded n = Kv ("border-radius", Printf.sprintf "%dpx" n)

let shadow_sm = Kv ("box-shadow", "0 1px 2px 0 rgba(0,0,0,0.25)")
let shadow_md = Kv ("box-shadow", "0 4px 6px -1px rgba(0,0,0,0.3)")
let shadow_lg = Kv ("box-shadow", "0 10px 15px -3px rgba(0,0,0,0.35)")
let shadow_xl = Kv ("box-shadow", "0 20px 25px -5px rgba(0,0,0,0.45)")
let shadow_glow c = Kv ("box-shadow", Printf.sprintf "0 0 24px -4px %s" (color_to_css c))
let opacity f = Kv ("opacity", Printf.sprintf "%.2f" f)
let backdrop_blur px = Kv ("backdrop-filter", Printf.sprintf "blur(%dpx)" px)

(* typography *)
let font_sans = Kv ("font-family", "Inter, system-ui, -apple-system, sans-serif")
let font_mono = Kv ("font-family", "'JetBrains Mono', ui-monospace, monospace")
let font_serif = Kv ("font-family", "Georgia, serif")

let text_xs = Kv ("font-size", "12px;line-height:16px")
let text_sm = Kv ("font-size", "14px;line-height:20px")
let text_base = Kv ("font-size", "16px;line-height:24px")
let text_lg = Kv ("font-size", "18px;line-height:28px")
let text_xl = Kv ("font-size", "20px;line-height:28px")
let text_2xl = Kv ("font-size", "24px;line-height:32px")
let text_3xl = Kv ("font-size", "30px;line-height:36px")
let text_4xl = Kv ("font-size", "36px;line-height:40px")
let font_size px = Kv ("font-size", Printf.sprintf "%dpx" px)

let font_normal = Kv ("font-weight", "400")
let font_medium = Kv ("font-weight", "500")
let font_semibold = Kv ("font-weight", "600")
let font_bold = Kv ("font-weight", "700")
let bold = font_bold
let italic = Kv ("font-style", "italic")
let underline = Kv ("text-decoration", "underline")
let no_underline = Kv ("text-decoration", "none")
let uppercase = Kv ("text-transform", "uppercase")
let lowercase = Kv ("text-transform", "lowercase")
let capitalize = Kv ("text-transform", "capitalize")
let tracking_wide = Kv ("letter-spacing", "0.05em")
let text_left = Kv ("text-align", "left")
let text_center = Kv ("text-align", "center")
let text_right = Kv ("text-align", "right")
let truncate = Kv ("overflow", "hidden;text-overflow:ellipsis;white-space:nowrap")

(* interactions, physics & animations *)
let cursor_pointer = Kv ("cursor", "pointer")
let cursor_not_allowed = Kv ("cursor", "not-allowed")
let select_none = Kv ("user-select", "none")
let transition_all = Kv ("transition", "all 180ms cubic-bezier(0.22, 1, 0.36, 1)")
let transition_spring = Kv ("transition", "transform 280ms cubic-bezier(0.34, 1.56, 0.64, 1), box-shadow 220ms ease")
let scale s = Kv ("transform", Printf.sprintf "scale(%.2f)" s)
let translate_y px = Kv ("transform", Printf.sprintf "translateY(%dpx)" px)

let animate_fade_in = Kv ("animation", "q_fade_in 260ms cubic-bezier(0.22,1,0.36,1) both")
let animate_slide_up = Kv ("animation", "q_slide_up 320ms cubic-bezier(0.22,1,0.36,1) both")
let animate_pulse = Kv ("animation", "q_pulse 2s cubic-bezier(0.4,0,0.6,1) infinite")

let hover rules = Hover rules
let focus rules = Focus rules
let active rules = Active rules
let dark rules = Dark rules
let sm rules = Media (640, rules)
let md rules = Media (768, rules)
let lg rules = Media (1024, rules)
let xl rules = Media (1280, rules)

(* 1-liner spring hover lift for cards & buttons *)
let hover_lift =
  Hover [ translate_y (-3); shadow_xl; border ~width:1 ~color:Palette.sky_500 () ]

let kvs_to_css_body styles =
  let buf = Buffer.create 64 in
  List.iter
    (function
      | Kv (k, v) ->
        if Buffer.length buf > 0 then Buffer.add_char buf ';';
        Buffer.add_string buf k;
        Buffer.add_char buf ':';
        Buffer.add_string buf v
      | _ -> ())
    styles;
  Buffer.contents buf

let compile_styles ~cls_prefix styles =
  let inline_str = kvs_to_css_body styles in
  let scoped_rules = ref [] in
  let idx = ref 0 in
  let emit_cls () =
    incr idx;
    Printf.sprintf "%s_%d" cls_prefix !idx
  in
  let classes = ref [] in
  List.iter
    (function
      | Kv _ -> ()
      | Hover inner ->
        let c = emit_cls () in
        classes := c :: !classes;
        scoped_rules := Printf.sprintf ".%s:hover{%s}" c (kvs_to_css_body inner) :: !scoped_rules
      | Focus inner ->
        let c = emit_cls () in
        classes := c :: !classes;
        scoped_rules := Printf.sprintf ".%s:focus{%s}" c (kvs_to_css_body inner) :: !scoped_rules
      | Active inner ->
        let c = emit_cls () in
        classes := c :: !classes;
        scoped_rules := Printf.sprintf ".%s:active{%s}" c (kvs_to_css_body inner) :: !scoped_rules
      | Dark inner ->
        let c = emit_cls () in
        classes := c :: !classes;
        scoped_rules := Printf.sprintf ".dark .%s{%s}" c (kvs_to_css_body inner) :: !scoped_rules
      | Media (min_w, inner) ->
        let c = emit_cls () in
        classes := c :: !classes;
        scoped_rules :=
          Printf.sprintf "@media(min-width:%dpx){.%s{%s}}" min_w c (kvs_to_css_body inner)
          :: !scoped_rules)
    styles;
  (inline_str, List.rev !classes, List.rev !scoped_rules)
