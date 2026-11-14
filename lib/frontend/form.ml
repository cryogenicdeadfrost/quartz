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

(* good enough email check without dragging in a 4000-char PCRE regex *)
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

(* quick form input component with built-in error text below it *)
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
