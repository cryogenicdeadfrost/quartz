type params = (string * string) list

type route_def = {
  pattern : string;
  title : string option;
  guard : (unit -> string option) option;
  layout : (Dom.node -> Dom.node) option;
  render : params -> Dom.node;
}

let active_path : string Signal.t = Signal.make "/"
let active_query : (string * string) list Signal.t = Signal.make []

let current_path () = active_path
let current_query () = active_query

let parse_query_str qs =
  if qs = "" then []
  else
    List.filter_map
      (fun pair ->
        match String.split_on_char '=' pair with
        | k :: v_rest when k <> "" -> Some (k, String.concat "=" v_rest)
        | _ -> None)
      (String.split_on_char '&' qs)

let navigate raw_target =
  let clean = if raw_target = "" then "/" else raw_target in
  let path_part, qs_part =
    match String.split_on_char '?' clean with
    | [] -> ("/", "")
    | [ p ] -> (if p = "" then "/" else p), ""
    | p :: rest -> (if p = "" then "/" else p), String.concat "?" rest
  in
  Signal.batch (fun () ->
    Signal.set active_path path_part;
    Signal.set active_query (parse_query_str qs_part))

let query_param key =
  List.assoc_opt key (Signal.get active_query)

let set_query key value =
  let cur = List.remove_assoc key (Signal.peek active_query) in
  let updated = if value = "" then cur else (key, value) :: cur in
  Signal.set active_query updated

let split_path p =
  let raw =
    match String.split_on_char '?' p with
    | hd :: _ -> hd
    | [] -> p
  in
  List.filter (fun s -> s <> "") (String.split_on_char '/' raw)

let match_pattern pat target =
  let p_segs = split_path pat in
  let t_segs = split_path target in
  (* support trailing wildcard /docs/* matching any depth *)
  let has_wild = List.exists (( = ) "*") p_segs in
  if not has_wild && List.length p_segs <> List.length t_segs then None
  else
    let rec loop ps ts acc =
      match ps, ts with
      | [], [] -> Some (List.rev acc)
      | [ "*" ], rest_t -> Some (List.rev (("wildcard", String.concat "/" rest_t) :: acc))
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

let route ?title ?guard ?layout pattern render =
  { pattern; title; guard; layout; render }

let with_layout layout_fn routes =
  List.map
    (fun r ->
      let merged_layout =
        match r.layout with
        | None -> Some layout_fn
        | Some inner -> Some (fun n -> layout_fn (inner n))
      in
      { r with layout = merged_layout })
    routes

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
         | None -> find rest
         | Some ps ->
           (match r.guard with
            | Some g ->
              (match g () with
               | Some redirect_to when redirect_to <> cur ->
                 navigate redirect_to;
                 find routes
               | _ ->
                 let v = r.render ps in
                 match r.layout with Some lf -> lf v | None -> v)
            | None ->
              let v = r.render ps in
              match r.layout with Some lf -> lf v | None -> v))
    in
    find routes)

let link ~to_ ?(active_style = Style.[ color Palette.sky_400; font_semibold ]) ?(style = []) kids =
  Dom.rx (fun () ->
    let cur = Signal.get active_path in
    let target_base =
      match String.split_on_char '?' to_ with hd :: _ -> hd | [] -> to_
    in
    let is_active = cur = target_base in
    let merged =
      Style.[ cursor_pointer; no_underline; transition_all ]
      @ style
      @ (if is_active then active_style else [])
    in
    Dom.a ~href:to_ ~style:merged ~on_click:(fun () -> navigate to_) kids)

(* automatic breadcrumb trail from current URL segments *)
let breadcrumbs () =
  Dom.rx (fun () ->
    let segs = split_path (Signal.get active_path) in
    let rec build prefix acc = function
      | [] -> List.rev acc
      | s :: rest ->
        let next_p = prefix ^ "/" ^ s in
        let item =
          link ~to_:next_p ~style:Style.[ text_xs; color Palette.slate_400; capitalize ] [ Dom.text s ]
        in
        build next_p (item :: Dom.span ~style:Style.[ text_xs; color Palette.slate_600 ] [ Dom.text "/" ] :: acc) rest
    in
    let home = link ~to_:"/" ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "Home" ] in
    Dom.div ~style:Style.[ flex_row; items_center; gap 8 ] (home :: build "" [] segs))

(* static route & link validator used by `quartz check` *)
let route_matches_any (routes : route_def list) target_url =
  List.exists (fun r -> Option.is_some (match_pattern r.pattern target_url)) routes

let audit_routes (routes : route_def list) (sample_paths : string list) (app : unit -> Dom.node) =
  let broken_links = ref [] in
  let checked_Links = ref [] in
  List.iter
    (fun p ->
      navigate p;
      let diag = Dom.diagnose (app ()) in
      List.iter
        (fun href ->
          if not (List.mem href !checked_Links) then (
            checked_Links := href :: !checked_Links;
            if not (route_matches_any routes href) then
              broken_links := (p, href) :: !broken_links
          ))
        diag.internal_links)
    sample_paths;
  (List.rev !checked_Links, List.rev !broken_links)
