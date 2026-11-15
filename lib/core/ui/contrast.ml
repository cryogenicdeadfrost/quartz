(* WCAG 2.1 + APCA Perceptual Contrast + OKLCH / OKLab Color Science Engine.
   Zero dependencies -- pure sRGB <-> Linear sRGB <-> OKLab <-> OKLCH math. *)

let clamp01 x = Float.max 0.0 (Float.min 1.0 x)
let clamp_byte f = max 0 (min 255 (int_of_float (Float.round f)))

let hex_char_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | _ -> 0

let parse_hex_byte s offset =
  (hex_char_val s.[offset] * 16) + hex_char_val s.[offset + 1]

(* OKLab <-> Linear sRGB matrices (Bjorn Ottosson's specification) *)
let linear_to_srgb_unit c =
  let c = clamp01 c in
  if c <= 0.0031308 then 12.92 *. c
  else (1.055 *. Float.pow c (1.0 /. 2.4)) -. 0.055

let srgb_unit_to_linear c =
  let c = clamp01 c in
  if c <= 0.04045 then c /. 12.92
  else Float.pow ((c +. 0.055) /. 1.055) 2.4

let oklch_to_rgb ~l ~c ~h : int * int * int =
  let rad = h *. Float.pi /. 180.0 in
  let a = c *. cos rad and b = c *. sin rad in
  let l_ = l +. (0.3963377774 *. a) +. (0.2158037573 *. b) in
  let m_ = l -. (0.1055613458 *. a) -. (0.0638541728 *. b) in
  let s_ = l -. (0.0894841775 *. a) -. (1.2914855480 *. b) in
  let l3 = l_ *. l_ *. l_ in
  let m3 = m_ *. m_ *. m_ in
  let s3 = s_ *. s_ *. s_ in
  let lr = (4.0767416621 *. l3) -. (3.3077115913 *. m3) +. (0.2309699292 *. s3) in
  let lg = (-1.2684380046 *. l3) +. (2.6097574011 *. m3) -. (0.3413193965 *. s3) in
  let lb = (-0.0041960863 *. l3) -. (0.7034186147 *. m3) +. (1.7076147010 *. s3) in
  ( clamp_byte (linear_to_srgb_unit lr *. 255.0),
    clamp_byte (linear_to_srgb_unit lg *. 255.0),
    clamp_byte (linear_to_srgb_unit lb *. 255.0) )

let rgb_to_oklch (r, g, b) : float * float * float =
  let lr = srgb_unit_to_linear (float_of_int r /. 255.0) in
  let lg = srgb_unit_to_linear (float_of_int g /. 255.0) in
  let lb = srgb_unit_to_linear (float_of_int b /. 255.0) in
  let l = 0.4122214708 *. lr +. 0.5363325363 *. lg +. 0.0514459929 *. lb in
  let m = 0.2119034982 *. lr +. 0.6806995451 *. lg +. 0.1073969566 *. lb in
  let s = 0.0883024619 *. lr +. 0.2817188376 *. lg +. 0.6299787005 *. lb in
  let l_ = Float.cbrt l and m_ = Float.cbrt m and s_ = Float.cbrt s in
  let cap_l = (0.2104542553 *. l_) +. (0.7936177850 *. m_) -. (0.0040720468 *. s_) in
  let a = (1.9779984951 *. l_) -. (2.4285922050 *. m_) +. (0.4505937099 *. s_) in
  let b_ = (0.0259040371 *. l_) +. (0.7827717662 *. m_) -. (0.8086757660 *. s_) in
  let c = sqrt ((a *. a) +. (b_ *. b_)) in
  let h_raw = atan2 b_ a *. 180.0 /. Float.pi in
  let h = if h_raw < 0.0 then h_raw +. 360.0 else h_raw in
  (cap_l, c, h)

let to_rgb (c : Style.color) : int * int * int =
  match c with
  | `Rgb (r, g, b) -> (r, g, b)
  | `Rgba (r, g, b, _) -> (r, g, b)
  | `Oklch (l, ch, h) -> oklch_to_rgb ~l ~c:ch ~h
  | `Oklcha (l, ch, h, _) -> oklch_to_rgb ~l ~c:ch ~h
  | `Var _ -> (248, 250, 252)
  | `Hex raw ->
    let s =
      if String.length raw > 0 && raw.[0] = '#' then
        String.sub raw 1 (String.length raw - 1)
      else raw
    in
    if String.length s = 6 then
      (parse_hex_byte s 0, parse_hex_byte s 2, parse_hex_byte s 4)
    else if String.length s = 3 then
      (hex_char_val s.[0] * 17, hex_char_val s.[1] * 17, hex_char_val s.[2] * 17)
    else (15, 23, 42)

let to_hex (c : Style.color) : string =
  let r, g, b = to_rgb c in
  Printf.sprintf "#%02x%02x%02x" r g b

(* WCAG 2.1 *)
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

let contrast_ratio (c1 : Style.color) (c2 : Style.color) : float =
  let l1 = relative_luminance c1 in
  let l2 = relative_luminance c2 in
  let hi = Float.max l1 l2 and lo = Float.min l1 l2 in
  (hi +. 0.05) /. (lo +. 0.05)

let passes_aa ?(large_text = false) fg bg =
  let ratio = contrast_ratio fg bg in
  if large_text then ratio >= 3.0 else ratio >= 4.5

let passes_aaa ?(large_text = false) fg bg =
  let ratio = contrast_ratio fg bg in
  if large_text then ratio >= 4.5 else ratio >= 7.0

(* APCA (Accessible Perceptual Contrast Algorithm, SAPC-8g / W3 0.0.98G-4g)
   Returns signed Lc contrast in roughly [-108, +106]. |Lc| >= 60 is readable body text,
   |Lc| >= 45 is large/heading text, |Lc| >= 75 is preferred body copy. *)
let apca_luminance (c : Style.color) : float =
  let r, g, b = to_rgb c in
  let ch v = Float.pow (float_of_int (max 0 (min 255 v)) /. 255.0) 2.4 in
  let y = (0.2126729 *. ch r) +. (0.7151522 *. ch g) +. (0.0721750 *. ch b) in
  if y < 0.022 then y +. Float.pow (0.022 -. y) 1.414 else y

let apca_lc ~fg ~bg : float =
  let y_txt = apca_luminance fg in
  let y_bg = apca_luminance bg in
  if Float.abs (y_bg -. y_txt) < 0.0005 then 0.0
  else
    let norm_bg, norm_txt, blk_th = (0.56, 0.57, 0.001) in
    ignore blk_th;
    if y_bg > y_txt then
      let sapc = (Float.pow y_bg norm_bg -. Float.pow y_txt norm_txt) *. 1.14 in
      let out = if sapc < 0.1 then 0.0 else sapc -. 0.027 in
      out *. 100.0
    else
      let sapc = (Float.pow y_bg 0.65 -. Float.pow y_txt 0.62) *. 1.14 in
      let out = if sapc > -0.1 then 0.0 else sapc +. 0.027 in
      out *. 100.0

let passes_apca ?(min_lc = 60.0) fg bg =
  Float.abs (apca_lc ~fg ~bg) >= min_lc

let pick_ink ?(light = Style.Palette.white) ?(dark = Style.Palette.slate_950) (bg : Style.color) : Style.color =
  let cr_light = contrast_ratio light bg in
  let cr_dark = contrast_ratio dark bg in
  if cr_light >= cr_dark then light else dark

let auto_text_color = pick_ink

(* Generate an 11-step Tailwind-like scale (50..950) in perceptual OKLCH space from any base color *)
let generate_scale (base : Style.color) : (int * Style.color) list =
  let _l0, c0, h0 = rgb_to_oklch (to_rgb base) in
  let stops =
    [
      (50, 0.97, 0.18);
      (100, 0.93, 0.30);
      (200, 0.87, 0.50);
      (300, 0.78, 0.72);
      (400, 0.70, 0.90);
      (500, 0.62, 1.00);
      (600, 0.54, 0.95);
      (700, 0.45, 0.84);
      (800, 0.37, 0.68);
      (900, 0.29, 0.52);
      (950, 0.20, 0.36);
    ]
  in
  List.map
    (fun (step, target_l, chroma_scale) ->
      let r, g, b = oklch_to_rgb ~l:target_l ~c:(c0 *. chroma_scale) ~h:h0 in
      (step, `Hex (Printf.sprintf "#%02x%02x%02x" r g b)))
    stops

let mix ?(t = 0.5) (c1 : Style.color) (c2 : Style.color) : Style.color =
  let r1, g1, b1 = to_rgb c1 in
  let r2, g2, b2 = to_rgb c2 in
  let lerp a b = int_of_float (Float.round (float_of_int a *. (1.0 -. t) +. (float_of_int b *. t))) in
  `Rgb (lerp r1 r2, lerp g1 g2, lerp b1 b2)

let tint ?(amount = 0.25) c = mix ~t:amount c Style.Palette.white
let shade ?(amount = 0.25) c = mix ~t:amount c Style.Palette.black
