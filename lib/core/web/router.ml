(* Type-Safe GADT Path Combinators + Dynamic Pattern Router + SEO Meta + HTTP Status Resolution. *)

type params = (string * string) list

type route_def = {
  pattern : string;
  title : string option;
  meta : Dom.page_meta option;
  guard : (unit -> string option) option;
  loader : (params -> unit) option;
  layout : (Dom.node -> Dom.node) option;
  render : params -> Dom.node;
}

type redirect_def = {
  from_path : string;
  to_path : string;
  status_code : int; (* 301 Permanent or 302 Temporary *)
}

type resolution =
  | Matched of { route : route_def; params : params; view : Dom.node }
  | Redirected of { status_code : int; location : string }
  | Not_found of { view : Dom.node }

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

let with_path raw_target (f : unit -> 'a) : 'a =
  navigate raw_target;
  f ()

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

let route ?title ?meta ?guard ?loader ?layout pattern render =
  { pattern; title; meta; guard; loader; layout; render }

let redirect ?(permanent = true) ~from_path ~to_path () =
  { from_path; to_path; status_code = if permanent then 301 else 302 }

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

(* Type-Safe GADT Path Combinators (`Router.Path`)
   Wrong argument type on `Path.href` or handler is a compile-time OCaml error! *)
module Path = struct
  type ('fn, 'out) t =
    | End : ('out, 'out) t
    | Lit : string * ('fn, 'out) t -> ('fn, 'out) t
    | Str : ('fn, 'out) t -> (string -> 'fn, 'out) t
    | Int : ('fn, 'out) t -> (int -> 'fn, 'out) t
    | Bool : ('fn, 'out) t -> (bool -> 'fn, 'out) t

  let s lit rest = Lit (lit, rest)
  let str rest = Str rest
  let int rest = Int rest
  let bool rest = Bool rest
  let nil = End

  let rec to_segments : type fn out. int -> (fn, out) t -> string list =
   fun idx -> function
     | End -> []
     | Lit (lit, rest) -> lit :: to_segments idx rest
     | Str rest -> Printf.sprintf ":str_%d" idx :: to_segments (idx + 1) rest
     | Int rest -> Printf.sprintf ":int_%d" idx :: to_segments (idx + 1) rest
     | Bool rest -> Printf.sprintf ":bool_%d" idx :: to_segments (idx + 1) rest

  let to_pattern p =
    match to_segments 0 p with
    | [] -> "/"
    | segs -> "/" ^ String.concat "/" segs

  let href (p : ('fn, string) t) : 'fn =
    let rec go : type fn. string list -> (fn, string) t -> fn =
     fun acc -> function
       | End -> (match List.rev acc with [] -> "/" | segs -> "/" ^ String.concat "/" segs)
       | Lit (lit, rest) -> go (lit :: acc) rest
       | Str rest -> fun v -> go (v :: acc) rest
       | Int rest -> fun v -> go (string_of_int v :: acc) rest
       | Bool rest -> fun v -> go ((if v then "true" else "false") :: acc) rest
    in
    go [] p

  let match_and_apply (type a) (p : ('fn, a) t) (handler : 'fn) (target : string) : a option =
    let segs = split_path target in
    let rec go : type fn. string list -> (fn, a) t -> fn -> a option =
     fun ts spec f ->
       match spec, ts with
       | End, [] -> Some f
       | Lit (lit, rest), t :: tr when lit = t -> go tr rest f
       | Str rest, t :: tr -> go tr rest (f t)
       | Int rest, t :: tr ->
         (match int_of_string_opt t with
          | Some v -> go tr rest (f v)
          | None -> None)
       | Bool rest, ("true" | "1") :: tr -> go tr rest (f true)
       | Bool rest, ("false" | "0") :: tr -> go tr rest (f false)
       | _ -> None
    in
    go segs p handler
end

let gadt_route ?title ?meta ?guard ?loader ?layout (path : ('fn, Dom.node) Path.t) (render : 'fn) : route_def =
  let pat = Path.to_pattern path in
  {
    pattern = pat;
    title;
    meta;
    guard;
    loader;
    layout;
    render =
      (fun _params ->
        let cur = Signal.peek active_path in
        match Path.match_and_apply path render cur with
        | Some v -> v
        | None -> default_404 ());
  }

let resolve ?(redirects = []) ?(not_found = default_404) (routes : route_def list) (raw_url : string) : resolution =
  let clean_path =
    match String.split_on_char '?' raw_url with
    | p :: _ when p <> "" -> p
    | _ -> "/"
  in
  match List.find_opt (fun r -> r.from_path = clean_path) redirects with
  | Some rd -> Redirected { status_code = rd.status_code; location = rd.to_path }
  | None ->
    navigate raw_url;
    let exec_route r ps =
      Option.iter (fun ld -> ld ps) r.loader;
      let raw_v = r.render ps in
      let view = match r.layout with Some lf -> lf raw_v | None -> raw_v in
      Matched { route = r; params = ps; view }
    in
    let rec find = function
      | [] -> Not_found { view = not_found () }
      | r :: rest ->
        (match match_pattern r.pattern clean_path with
         | None -> find rest
         | Some ps ->
           (match r.guard with
            | Some g ->
              (match g () with
               | Some redirect_to when redirect_to <> clean_path ->
                 Redirected { status_code = 302; location = redirect_to }
               | _ -> exec_route r ps)
            | None -> exec_route r ps))
    in
    find routes

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

let link ~to_ ?(prefetch = true) ?(active_style = Style.[ color Palette.sky_400; font_semibold ]) ?(style = []) kids =
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
    Dom.a ~href:to_ ~prefetch ~style:merged ~on_click:(fun () -> navigate to_) kids)

(* Next.js-style Parallel & Intercepting @modal Slots *)
let parallel_slot ~name (children : Dom.node list) : Dom.node =
  Dom.div ~attrs:[ Dom.attr "data-q-slot" name ] children

let modal_slot ~is_open ~on_close (children : Dom.node list) : Dom.node =
  if not is_open then Dom.empty
  else
    Dom.div
      ~attrs:[ Dom.attr "role" "dialog"; Dom.attr "aria-modal" "true"; Dom.attr "data-q-trap-focus" "1"; Dom.attr "data-q-slot" "modal" ]
      ~style:
        Style.[
          raw_css "position" "fixed";
          raw_css "inset" "0";
          raw_css "z-index" "50";
          raw_css "background" "rgba(2, 6, 23, 0.72)";
          raw_css "backdrop-filter" "blur(6px)";
          flex_col;
          items_center;
          justify_center;
          p 24;
        ]
      [
        Dom.div
          ~style:
            Style.[
              bg Palette.slate_900;
              border ~width:1 ~color:Palette.slate_700 ();
              rounded_lg;
              p 24;
              max_w 560;
              w_full;
            ]
          (Dom.div ~style:Style.[ flex_row; justify_end; mb 12 ]
             [ Dom.button ~on_click:on_close ~attrs:[ Dom.attr "aria-label" "Close modal" ] [ Dom.text "Close" ] ]
           :: children);
      ]

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
        build next_p (item :: Dom.span ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "/" ] :: acc) rest
    in
    let home = link ~to_:"/" ~style:Style.[ text_xs; color Palette.slate_400 ] [ Dom.text "Home" ] in
    Dom.div ~style:Style.[ flex_row; items_center; gap 8 ] (home :: build "" [] segs))

let route_matches_any (routes : route_def list) target_url =
  List.exists (fun r -> Option.is_some (match_pattern r.pattern target_url)) routes

let audit_routes (routes : route_def list) (sample_paths : string list) (app : unit -> Dom.node) =
  let broken_links = ref [] in
  let checked_links = ref [] in
  List.iter
    (fun p ->
      navigate p;
      let diag = Dom.diagnose (app ()) in
      List.iter
        (fun href ->
          if not (List.mem href !checked_links) then (
            checked_links := href :: !checked_links;
            if not (route_matches_any routes href) then
              broken_links := (p, href) :: !broken_links
          ))
        diag.internal_links)
    sample_paths;
  (List.rev !checked_links, List.rev !broken_links)
