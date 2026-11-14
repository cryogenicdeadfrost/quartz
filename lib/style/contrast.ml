(* WCAG 2.1 sRGB Relative Luminance & Color Contrast Math Engine.
   Because squinting at low-contrast grey-on-grey text is a war crime. *)

let hex_char_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | _ -> 0

let parse_hex_byte s offset =
  (hex_char_val s.[offset] * 16) + hex_char_val s.[offset + 1]

let to_rgb (c : Style.color) : int * int * int =
  match c with
  | `Rgb (r, g, b) -> (r, g, b)
  | `Rgba (r, g, b, _) -> (r, g, b)
  | `Var _ -> (248, 250, 252) (* fallback slate_50 for dynamic vars *)
  | `Hex raw ->
    let s =
      if String.length raw > 0 && raw.[0] = '#' then
        String.sub raw 1 (String.length raw - 1)
      else raw
    in
    if String.length s = 6 then
      (parse_hex_byte s 0, parse_hex_byte s 2, parse_hex_byte s 4)
    else if String.length s = 3 then
      let r = hex_char_val s.[0] * 17 in
      let g = hex_char_val s.[1] * 17 in
      let b = hex_char_val s.[2] * 17 in
      (r, g, b)
    else (15, 23, 42)

(* sRGB channel linearization per WCAG 2.1 spec:
   if C_srgb <= 0.03928 then C / 12.92 else ((C + 0.055) / 1.055) ^ 2.4 *)
let linearize_channel (byte_v : int) : float =
  let c = float_of_int (max 0 (min 255 byte_v)) /. 255.0 in
  if c <= 0.03928 then c /. 12.92
  else Float.pow ((c +. 0.055) /. 1.055) 2.4

let relative_luminance (c : Style.color) : float =
  let r, g, b = to_rgb c in
  let rl = linearize_channel r in
  let gl = linearize_channel g in
  let bl = linearize_channel b in
  (0.2126 *. rl) +. (0.7152 *. gl) +. (0.0722 *. bl)

(* Contrast Ratio CR = (L_lighter + 0.05) / (L_darker + 0.05) -> ranges from 1.0 to 21.0 *)
let contrast_ratio (c1 : Style.color) (c2 : Style.color) : float =
  let l1 = relative_luminance c1 in
  let l2 = relative_luminance c2 in
  let hi = Float.max l1 l2 in
  let lo = Float.min l1 l2 in
  (hi +. 0.05) /. (lo +. 0.05)

let passes_aa ?(large_text = false) fg bg =
  let ratio = contrast_ratio fg bg in
  if large_text then ratio >= 3.0 else ratio >= 4.5

let passes_aaa ?(large_text = false) fg bg =
  let ratio = contrast_ratio fg bg in
  if large_text then ratio >= 4.5 else ratio >= 7.0

(* Automatically picks crisp dark or white text ink for any background color *)
let pick_ink ?(light = Style.Palette.white) ?(dark = Style.Palette.slate_950) (bg : Style.color) : Style.color =
  let cr_light = contrast_ratio light bg in
  let cr_dark = contrast_ratio dark bg in
  if cr_light >= cr_dark then light else dark

let auto_text_color = pick_ink

(* Blend two colors by ratio t in [0.0, 1.0] *)
let mix ?(t = 0.5) (c1 : Style.color) (c2 : Style.color) : Style.color =
  let r1, g1, b1 = to_rgb c1 in
  let r2, g2, b2 = to_rgb c2 in
  let lerp a b = int_of_float (Float.round (float_of_int a *. (1.0 -. t) +. (float_of_int b *. t))) in
  `Rgb (lerp r1 r2, lerp g1 g2, lerp b1 b2)

let tint ?(amount = 0.25) c = mix ~t:amount c Style.Palette.white
let shade ?(amount = 0.25) c = mix ~t:amount c Style.Palette.black
