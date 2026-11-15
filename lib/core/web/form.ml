(* Declarative Validation, Reactive Form Fields, Honeypot Anti-Spam,
   and Server Schema Runner for Quartz Forms. *)

type 'a validator = string -> ('a, string) result

let non_empty ?(msg = "This field cannot be empty") (s : string) =
  let t = String.trim s in
  if t = "" then Error msg else Ok t

let min_len n ?msg (s : string) =
  let m = Option.value ~default:(Printf.sprintf "Must be at least %d characters" n) msg in
  if String.length (String.trim s) < n then Error m else Ok s

let max_len n ?msg (s : string) =
  let m = Option.value ~default:(Printf.sprintf "Must be at most %d characters" n) msg in
  if String.length s > n then Error m else Ok s

let email ?(msg = "Enter a valid email address") (s : string) =
  let s = String.trim s in
  match String.split_on_char '@' s with
  | [ user; domain ] when user <> "" && String.contains domain '.' && domain.[0] <> '.' ->
    Ok s
  | _ -> Error msg

let int_range ~min ~max ?msg (s : string) =
  let m = Option.value ~default:(Printf.sprintf "Enter an integer between %d and %d" min max) msg in
  match int_of_string_opt (String.trim s) with
  | Some v when v >= min && v <= max -> Ok v
  | _ -> Error m

let float_pos ?(msg = "Enter a positive number") (s : string) =
  match float_of_string_opt (String.trim s) with
  | Some v when not (Float.is_nan v) && v > 0.0 -> Ok v
  | _ -> Error msg

let chain (v1 : string validator) (v2 : 'a validator) : 'a validator =
  fun s ->
    match v1 s with
    | Error e -> Error e
    | Ok s' -> v2 s'

(* Pure-OCaml regex/pattern matcher supporting ^, $, ., *, +, ?, character classes [a-z0-9_], and escapes *)
let matches_pattern ~(pattern : string) (input : string) : bool =
  let plen = String.length pattern in
  let anchored_start = plen > 0 && pattern.[0] = '^' in
  let pat = if anchored_start then String.sub pattern 1 (plen - 1) else pattern in
  let match_class p_idx ch =
    let len = String.length pat in
    let rec find_close j neg items =
      if j >= len then None
      else if pat.[j] = ']' && j > p_idx + (if neg then 2 else 1) then
        Some (neg, items, j + 1)
      else if j + 2 < len && pat.[j + 1] = '-' && pat.[j + 2] <> ']' then
        find_close (j + 3) neg (`Range (pat.[j], pat.[j + 2]) :: items)
      else
        find_close (j + 1) neg (`Char pat.[j] :: items)
    in
    let neg, start =
      if p_idx + 1 < len && pat.[p_idx + 1] = '^' then (true, p_idx + 2)
      else (false, p_idx + 1)
    in
    match find_close start neg [] with
    | None -> (ch = '[', p_idx + 1)
    | Some (is_neg, items, next_p) ->
      let hit =
        List.exists
          (function
            | `Char c -> ch = c
            | `Range (lo, hi) -> ch >= lo && ch <= hi)
          items
      in
      ((if is_neg then not hit else hit), next_p)
  in
  let match_atom p_idx ch =
    let len = String.length pat in
    if p_idx >= len then None
    else
      match pat.[p_idx] with
      | '\\' when p_idx + 1 < len ->
        let esc = pat.[p_idx + 1] in
        let ok =
          match esc with
          | 'd' -> ch >= '0' && ch <= '9'
          | 'w' -> (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch = '_'
          | 's' -> ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'
          | lit -> ch = lit
        in
        Some (ok, p_idx + 2)
      | '[' -> Some (match_class p_idx ch)
      | '.' -> Some (true, p_idx + 1)
      | lit -> Some (ch = lit, p_idx + 1)
  in
  let atom_span p_idx =
    let len = String.length pat in
    if p_idx >= len then (p_idx, '\000')
    else
      let next_p =
        match pat.[p_idx] with
        | '\\' when p_idx + 1 < len -> p_idx + 2
        | '[' ->
          let _, np = match_class p_idx 'a' in
          np
        | _ -> p_idx + 1
      in
      if next_p < len && (pat.[next_p] = '*' || pat.[next_p] = '+' || pat.[next_p] = '?') then
        (next_p + 1, pat.[next_p])
      else (next_p, '\000')
  in
  let ilen = String.length input in
  let plen_p = String.length pat in
  let rec match_here p_idx i_idx =
    if p_idx >= plen_p then true
    else if pat.[p_idx] = '$' && p_idx = plen_p - 1 then i_idx = ilen
    else
      let after_quant, quant = atom_span p_idx in
      match quant with
      | '*' -> match_star p_idx after_quant i_idx
      | '+' ->
        if i_idx < ilen then
          match match_atom p_idx input.[i_idx] with
          | Some (true, _) -> match_star p_idx after_quant (i_idx + 1)
          | _ -> false
        else false
      | '?' ->
        (i_idx < ilen &&
         match match_atom p_idx input.[i_idx] with
         | Some (true, _) -> match_here after_quant (i_idx + 1)
         | _ -> false)
        || match_here after_quant i_idx
      | _ ->
        if i_idx >= ilen then false
        else
          match match_atom p_idx input.[i_idx] with
          | Some (true, np) -> match_here np (i_idx + 1)
          | _ -> false
  and match_star p_idx after_quant i_idx =
    if match_here after_quant i_idx then true
    else if i_idx < ilen then
      match match_atom p_idx input.[i_idx] with
      | Some (true, _) -> match_star p_idx after_quant (i_idx + 1)
      | _ -> false
    else false
  in
  if anchored_start then match_here 0 0
  else
    let rec scan i =
      if i > ilen then false
      else if match_here 0 i then true
      else scan (i + 1)
    in
    scan 0

let pattern ~pattern ?msg (s : string) =
  let m = Option.value ~default:(Printf.sprintf "Value does not match pattern %s" pattern) msg in
  if matches_pattern ~pattern s then Ok s else Error m

type 'a field = {
  raw : string Signal.t;
  touched : bool Signal.t;
  validator : 'a validator;
}

let create_field ?(initial = "") validator =
  { raw = Signal.make initial; touched = Signal.make false; validator }

let value f = Signal.get f.raw

let set_value f s =
  Signal.set f.touched true;
  Signal.set f.raw s

let reset_field ?(initial = "") f =
  Signal.set f.touched false;
  Signal.set f.raw initial

let validate f = f.validator (Signal.get f.raw)

let error f =
  if not (Signal.get f.touched) then None
  else
    match validate f with
    | Ok _ -> None
    | Error e -> Some e

(* Hidden honeypot field against automated form spam (works with 0 JS) *)
let honeypot ?(name = "_hp_company_url") () =
  Dom.input
    ~name
    ~type_:"text"
    ~value:""
    ~style:Style.[ raw_css "position" "absolute"; raw_css "left" "-9999px"; raw_css "opacity" "0" ]
    ~attrs:[ Dom.attr "tabindex" "-1"; Dom.attr "autocomplete" "off"; Dom.attr "aria-hidden" "true"; Dom.attr "aria-label" "Leave blank" ]
    ()

let check_honeypot ?(name = "_hp_company_url") (params : (string * string) list) : (unit, string) result =
  match List.assoc_opt name params with
  | Some v when String.trim v <> "" -> Error "Bot submission rejected"
  | _ -> Ok ()

let validate_params (rules : (string * (string -> (string, string) result)) list) (params : (string * string) list)
    : ((string * string) list, (string * string) list) result =
  let oks = ref [] and errs = ref [] in
  List.iter
    (fun (field_name, check_fn) ->
      let raw = Option.value ~default:"" (List.assoc_opt field_name params) in
      match check_fn raw with
      | Ok clean -> oks := (field_name, clean) :: !oks
      | Error msg -> errs := (field_name, msg) :: !errs)
    rules;
  if !errs = [] then Ok (List.rev !oks) else Error (List.rev !errs)

let view_input ~label ?placeholder ?(type_ = "text") (f : 'a field) =
  Dom.rx (fun () ->
    let cur = Signal.get f.raw in
    let err_opt = error f in
    let border_col =
      match err_opt with
      | Some _ -> Style.Palette.rose_500
      | None -> Style.Palette.slate_700
    in
    Dom.div
      ~style:Style.[ flex_col; gap 6; w_full ]
      [
        Dom.label
          ~style:Style.[ text_sm; font_medium; color Palette.slate_300 ]
          [ Dom.text label ];
        Dom.input
          ~name:label
          ~type_
          ~value:cur
          ?placeholder
          ~style:
            Style.[
              p 10;
              rounded_md;
              bg Palette.slate_900;
              color Palette.white;
              border ~width:1 ~color:border_col ();
            ]
          ~on_input:(fun v -> set_value f v)
          ();
        (match err_opt with
         | None -> Dom.empty
         | Some msg ->
           Dom.span ~style:Style.[ text_xs; color Palette.rose_400 ] [ Dom.text msg ]);
      ])
