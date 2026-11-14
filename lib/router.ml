type params = (string * string) list

type route_def = {
  pattern : string;
  render : params -> Dom.node;
}

let active_path : string Signal.t = Signal.make "/"

let current_path () = active_path

let navigate path =
  let clean = if path = "" then "/" else path in
  Signal.set active_path clean

let split_path p =
  let raw =
    match String.split_on_char '?' p with
    | hd :: _ -> hd
    | [] -> p
  in
  List.filter (fun s -> s <> "") (String.split_on_char '/' raw)

(* matches /users/:id/settings against /users/42/settings *)
let match_pattern pat target =
  let p_segs = split_path pat in
  let t_segs = split_path target in
  if List.length p_segs <> List.length t_segs then None
  else
    let rec loop ps ts acc =
      match ps, ts with
      | [], [] -> Some (List.rev acc)
      | p :: pr, t :: tr ->
        if String.length p > 1 && p.[0] = ':' then
          let key = String.sub p 1 (String.length p - 1) in
          loop pr tr ((key, t) :: acc)
        else if p = "*" then
          loop pr tr (("*", t) :: acc)
        else if p = t then
          loop pr tr acc
        else None
      | _ -> None
    in
    loop p_segs t_segs []

let param key (ps : params) = List.assoc_opt key ps

let route pattern render = { pattern; render }

let default_404 () =
  Dom.div
    ~style:Style.[ flex_col; items_center; justify_center; p 48; gap 12; color Palette.slate_300 ]
    [
      Dom.h1 ~style:Style.[ text_4xl; bold; color Palette.rose_400 ] [ Dom.text "404" ];
      Dom.p [ Dom.text "Page wandered off into the void." ];
    ]

let router ?(not_found = default_404) (routes : route_def list) =
  Dom.rx (fun () ->
    let cur = Signal.get active_path in
    let rec find = function
      | [] -> not_found ()
      | r :: rest ->
        (match match_pattern r.pattern cur with
         | Some ps -> r.render ps
         | None -> find rest)
    in
    find routes)

let link ~to_ ?(active_style = Style.[ color Palette.sky_400; font_semibold ]) ?(style = []) kids =
  Dom.rx (fun () ->
    let is_active = Signal.get active_path = to_ in
    let merged =
      Style.[ cursor_pointer; no_underline ] @ style @ (if is_active then active_style else [])
    in
    Dom.a ~href:to_ ~style:merged ~on_click:(fun () -> navigate to_) kids)
