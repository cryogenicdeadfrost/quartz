(* ES2025/2026 & Node.js standard utilities in pure OCaml (zero Unix deps).
   Url, Search_params, Iter (Seq), Set_ops, Regex_util, Temporal, Emitter, Path, Env_parser. *)

module Search_params = struct
  type t = (string * string) list

  let hex_of_nibble n =
    if n < 10 then Char.chr (Char.code '0' + n)
    else Char.chr (Char.code 'A' + n - 10)

  let is_unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
    | _ -> false

  let encode_component s =
    let b = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        if is_unreserved c then Buffer.add_char b c
        else (
          let code = Char.code c in
          Buffer.add_char b '%';
          Buffer.add_char b (hex_of_nibble (code lsr 4));
          Buffer.add_char b (hex_of_nibble (code land 0xF))
        ))
      s;
    Buffer.contents b

  let decode_component = Http.url_decode

  let parse qs : t =
    let raw =
      if String.length qs > 0 && qs.[0] = '?' then String.sub qs 1 (String.length qs - 1)
      else qs
    in
    if raw = "" then []
    else
      List.filter_map
        (fun pair ->
          if pair = "" then None
          else
            match String.index_opt pair '=' with
            | None -> Some (decode_component pair, "")
            | Some i ->
              let k = decode_component (String.sub pair 0 i) in
              let v = decode_component (String.sub pair (i + 1) (String.length pair - i - 1)) in
              Some (k, v))
        (String.split_on_char '&' raw)

  let get key (ps : t) = List.assoc_opt key ps

  let get_all key (ps : t) =
    List.filter_map (fun (k, v) -> if k = key then Some v else None) ps

  let has key (ps : t) = List.exists (fun (k, _) -> k = key) ps
  let append key value (ps : t) : t = ps @ [ (key, value) ]
  let delete key (ps : t) : t = List.filter (fun (k, _) -> k <> key) ps
  let set key value (ps : t) : t = delete key ps @ [ (key, value) ]
  let entries (ps : t) = ps

  let to_string (ps : t) =
    String.concat "&"
      (List.map (fun (k, v) -> encode_component k ^ "=" ^ encode_component v) ps)
end

module Url = struct
  type t = {
    scheme : string;
    host : string;
    port : int option;
    path : string;
    query : Search_params.t;
    hash : string option;
  }

  let parse raw : (t, string) result =
    let s = String.trim raw in
    match String.index_opt s ':' with
    | None -> Error ("Missing URL scheme: " ^ raw)
    | Some i when i + 2 >= String.length s || String.sub s (i + 1) 2 <> "//" ->
      Error ("Invalid URL authority: " ^ raw)
    | Some i ->
      let scheme = String.lowercase_ascii (String.sub s 0 i) in
      let rest = String.sub s (i + 3) (String.length s - i - 3) in
      let before_hash, hash =
        match String.index_opt rest '#' with
        | None -> (rest, None)
        | Some h ->
          ( String.sub rest 0 h,
            Some (String.sub rest (h + 1) (String.length rest - h - 1)) )
      in
      let before_q, qs =
        match String.index_opt before_hash '?' with
        | None -> (before_hash, "")
        | Some q ->
          ( String.sub before_hash 0 q,
            String.sub before_hash (q + 1) (String.length before_hash - q - 1) )
      in
      let authority, path =
        match String.index_opt before_q '/' with
        | None -> (before_q, "/")
        | Some p ->
          ( String.sub before_q 0 p,
            String.sub before_q p (String.length before_q - p) )
      in
      let host, port =
        match String.rindex_opt authority ':' with
        | None -> (authority, None)
        | Some c ->
          let h = String.sub authority 0 c in
          let p_str = String.sub authority (c + 1) (String.length authority - c - 1) in
          (h, int_of_string_opt p_str)
      in
      if host = "" then Error ("Empty URL host: " ^ raw)
      else Ok { scheme; host; port; path; query = Search_params.parse qs; hash }

  let to_string u =
    let port_s = match u.port with Some p -> ":" ^ string_of_int p | None -> "" in
    let qs = Search_params.to_string u.query in
    let q_s = if qs = "" then "" else "?" ^ qs in
    let h_s = match u.hash with Some h -> "#" ^ h | None -> "" in
    Printf.sprintf "%s://%s%s%s%s%s" u.scheme u.host port_s u.path q_s h_s

  let normalize_segments segs =
    let rec go acc = function
      | [] -> List.rev acc
      | "" :: rest | "." :: rest -> go acc rest
      | ".." :: rest ->
        let acc' = match acc with _ :: tl -> tl | [] -> [] in
        go acc' rest
      | s :: rest -> go (s :: acc) rest
    in
    "/" ^ String.concat "/" (go [] segs)

  let resolve ~(base : t) (rel : string) : (t, string) result =
    match parse rel with
    | Ok abs -> Ok abs
    | Error _ ->
      let before_hash, hash =
        match String.index_opt rel '#' with
        | None -> (rel, None)
        | Some h -> (String.sub rel 0 h, Some (String.sub rel (h + 1) (String.length rel - h - 1)))
      in
      let rel_path, qs =
        match String.index_opt before_hash '?' with
        | None -> (before_hash, "")
        | Some q ->
          ( String.sub before_hash 0 q,
            String.sub before_hash (q + 1) (String.length before_hash - q - 1) )
      in
      let merged_path =
        if rel_path = "" then base.path
        else if rel_path.[0] = '/' then normalize_segments (String.split_on_char '/' rel_path)
        else
          let base_dir =
            match String.rindex_opt base.path '/' with
            | None -> ""
            | Some idx -> String.sub base.path 0 idx
          in
          normalize_segments (String.split_on_char '/' (base_dir ^ "/" ^ rel_path))
      in
      Ok { base with path = merged_path; query = Search_params.parse qs; hash }
end

module Iter = struct
  type 'a t = 'a Seq.t

  let of_list = List.to_seq
  let to_list = List.of_seq
  let map = Seq.map
  let filter = Seq.filter
  let flat_map = Seq.flat_map
  let reduce = Seq.fold_left
  let for_each = Seq.iter

  let rec take n seq () =
    if n <= 0 then Seq.Nil
    else
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (x, rest) -> Seq.Cons (x, take (n - 1) rest)

  let rec drop n seq () =
    if n <= 0 then seq ()
    else
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (_, rest) -> drop (n - 1) rest ()

  let rec find pred seq =
    match seq () with
    | Seq.Nil -> None
    | Seq.Cons (x, rest) -> if pred x then Some x else find pred rest

  let some pred seq = Option.is_some (find pred seq)
  let every pred seq = not (some (fun x -> not (pred x)) seq)

  let rec zip s1 s2 () =
    match (s1 (), s2 ()) with
    | Seq.Cons (a, r1), Seq.Cons (b, r2) -> Seq.Cons ((a, b), zip r1 r2)
    | _ -> Seq.Nil

  let chunk size seq =
    let n = max 1 size in
    let rec pull acc k s =
      if k = 0 then (List.rev acc, s)
      else
        match s () with
        | Seq.Nil -> (List.rev acc, Seq.empty)
        | Seq.Cons (x, rest) -> pull (x :: acc) (k - 1) rest
    in
    let rec next s () =
      match s () with
      | Seq.Nil -> Seq.Nil
      | _ ->
        let grp, rest = pull [] n s in
        Seq.Cons (grp, next rest)
    in
    next seq

  let group_by (key_fn : 'a -> 'k) (seq : 'a t) : ('k * 'a list) list =
    let tbl = Hashtbl.create 16 in
    let order = ref [] in
    Seq.iter
      (fun x ->
        let k = key_fn x in
        match Hashtbl.find_opt tbl k with
        | Some xs -> Hashtbl.replace tbl k (x :: xs)
        | None ->
          order := k :: !order;
          Hashtbl.add tbl k [ x ])
      seq;
    List.rev_map (fun k -> (k, List.rev (Hashtbl.find tbl k))) !order
end

module Set_ops = struct
  let dedup xs =
    let seen = Hashtbl.create (List.length xs) in
    List.filter
      (fun x ->
        if Hashtbl.mem seen x then false
        else (
          Hashtbl.add seen x ();
          true
        ))
      xs

  let union a b = dedup (a @ b)
  let intersection a b = dedup (List.filter (fun x -> List.mem x b) a)
  let difference a b = dedup (List.filter (fun x -> not (List.mem x b)) a)
  let symmetric_difference a b = union (difference a b) (difference b a)
  let is_subset_of a b = List.for_all (fun x -> List.mem x b) a
  let is_superset_of a b = is_subset_of b a
  let is_disjoint_from a b = List.for_all (fun x -> not (List.mem x b)) a
end

module Regex_util = struct
  (* ES2025 RegExp.escape *)
  let escape s =
    let b = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with
        | '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '/' ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
        | _ -> Buffer.add_char b c)
      s;
    Buffer.contents b
end

module Temporal = struct
  module Duration = struct
    type t = { total_ms : int64 }

    let make ?(days = 0) ?(hours = 0) ?(minutes = 0) ?(seconds = 0) ?(ms = 0) () : t =
      let total =
        Int64.(
          add
            (mul (of_int days) 86_400_000L)
            (add
               (mul (of_int hours) 3_600_000L)
               (add (mul (of_int minutes) 60_000L) (add (mul (of_int seconds) 1_000L) (of_int ms)))))
      in
      { total_ms = total }

    let to_ms d = d.total_ms
    let to_seconds d = Int64.to_float d.total_ms /. 1000.0
    let add a b = { total_ms = Int64.add a.total_ms b.total_ms }
    let sub a b = { total_ms = Int64.sub a.total_ms b.total_ms }
    let negate d = { total_ms = Int64.neg d.total_ms }
  end

  module Instant = struct
    type t = { epoch_ms : int64 }

    let of_epoch_ms epoch_ms = { epoch_ms }
    let to_epoch_ms i = i.epoch_ms
    let now ?(clock = Io.Clock_hook.now_s) () = { epoch_ms = Int64.of_float (clock () *. 1000.0) }
    let add i (d : Duration.t) = { epoch_ms = Int64.add i.epoch_ms d.total_ms }
    let since ~other i : Duration.t = { total_ms = Int64.sub i.epoch_ms other.epoch_ms }
    let compare a b = Int64.compare a.epoch_ms b.epoch_ms

    let to_iso i =
      let secs = Int64.div i.epoch_ms 1000L in
      I18n.to_iso8601 secs
  end

  module Plain_date = struct
    type t = { year : int; month : int; day : int }

    let days_from_civil y m d =
      let y' = if m <= 2 then y - 1 else y in
      let era = (if y' >= 0 then y' else y' - 399) / 400 in
      let yoe = y' - (era * 400) in
      let doy = ((153 * (m + (if m > 2 then -3 else 9))) + 2) / 5 + d - 1 in
      let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
      (era * 146097) + doe - 719468

    let civil_from_days z =
      let z = z + 719468 in
      let era = (if z >= 0 then z else z - 146096) / 146097 in
      let doe = z - (era * 146097) in
      let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
      let y = yoe + (era * 400) in
      let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
      let mp = ((5 * doy) + 2) / 153 in
      let d = doy - (((153 * mp) + 2) / 5) + 1 in
      let m = mp + (if mp < 10 then 3 else -9) in
      let year = y + (if m <= 2 then 1 else 0) in
      { year; month = m; day = d }

    let make ~year ~month ~day =
      if month < 1 || month > 12 || day < 1 || day > 31 then Error "Invalid calendar date"
      else Ok { year; month; day }

    let add_days d n = civil_from_days (days_from_civil d.year d.month d.day + n)
    let days_between a b = days_from_civil b.year b.month b.day - days_from_civil a.year a.month a.day

    let compare a b =
      Int.compare
        (days_from_civil a.year a.month a.day)
        (days_from_civil b.year b.month b.day)

    let to_iso d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day

    let of_iso s =
      match String.split_on_char '-' (String.trim s) with
      | [ y; m; d ] ->
        (match (int_of_string_opt y, int_of_string_opt m, int_of_string_opt d) with
         | Some year, Some month, Some day -> make ~year ~month ~day
         | _ -> Error ("Invalid ISO date: " ^ s))
      | _ -> Error ("Invalid ISO date: " ^ s)
  end
end

module Emitter = struct
  type 'a listener = {
    id : int;
    once : bool;
    fn : 'a -> unit;
  }

  type 'a t = {
    mutable next_id : int;
    events : (string, 'a listener list) Hashtbl.t;
  }

  let create () : 'a t = { next_id = 1; events = Hashtbl.create 16 }

  let add_listener ~once em event fn =
    let id = em.next_id in
    em.next_id <- id + 1;
    let cur = Option.value ~default:[] (Hashtbl.find_opt em.events event) in
    Hashtbl.replace em.events event (cur @ [ { id; once; fn } ]);
    id

  let on em event fn = add_listener ~once:false em event fn
  let once em event fn = add_listener ~once:true em event fn

  let off em event id =
    match Hashtbl.find_opt em.events event with
    | None -> ()
    | Some ls -> Hashtbl.replace em.events event (List.filter (fun l -> l.id <> id) ls)

  let emit em event payload =
    match Hashtbl.find_opt em.events event with
    | None -> ()
    | Some ls ->
      let keep = List.filter (fun l -> not l.once) ls in
      Hashtbl.replace em.events event keep;
      List.iter (fun l -> l.fn payload) ls

  let listener_count em event =
    match Hashtbl.find_opt em.events event with None -> 0 | Some ls -> List.length ls

  let remove_all em ?event () =
    match event with
    | Some ev -> Hashtbl.remove em.events ev
    | None -> Hashtbl.clear em.events
end

module Path = struct
  let to_posix s = String.map (fun c -> if c = '\\' then '/' else c) s

  let is_absolute s =
    let p = to_posix s in
    (String.length p > 0 && p.[0] = '/')
    || (String.length p >= 2 && p.[1] = ':')

  let normalize raw =
    let p = to_posix raw in
    let leading_slash = String.length p > 0 && p.[0] = '/' in
    let segs = String.split_on_char '/' p in
    let rec go acc = function
      | [] -> List.rev acc
      | "" :: rest | "." :: rest -> go acc rest
      | ".." :: rest ->
        (match acc with
         | x :: tl when x <> ".." -> go tl rest
         | _ ->
           if leading_slash then go acc rest
           else go (".." :: acc) rest)
      | s :: rest -> go (s :: acc) rest
    in
    let parts = go [] segs in
    let joined = String.concat "/" parts in
    if leading_slash then "/" ^ joined
    else if joined = "" then "."
    else joined

  let join parts =
    normalize (String.concat "/" (List.filter (fun s -> s <> "") parts))

  let dirname p =
    let n = normalize p in
    match String.rindex_opt n '/' with
    | None -> "."
    | Some 0 -> "/"
    | Some i -> String.sub n 0 i

  let basename p =
    let n = normalize p in
    match String.rindex_opt n '/' with
    | None -> n
    | Some i -> String.sub n (i + 1) (String.length n - i - 1)

  let extname p =
    let b = basename p in
    match String.rindex_opt b '.' with
    | Some i when i > 0 -> String.sub b i (String.length b - i)
    | _ -> ""

  let relative ~from ~to_ =
    let a = String.split_on_char '/' (normalize from) |> List.filter (( <> ) "") in
    let b = String.split_on_char '/' (normalize to_) |> List.filter (( <> ) "") in
    let rec drop_common xs ys =
      match (xs, ys) with
      | x :: xr, y :: yr when x = y -> drop_common xr yr
      | _ -> (xs, ys)
    in
    let rem_a, rem_b = drop_common a b in
    let ups = List.init (List.length rem_a) (fun _ -> "..") in
    match ups @ rem_b with
    | [] -> "."
    | parts -> String.concat "/" parts
end

module Env_parser = struct
  let strip_quotes v =
    let n = String.length v in
    if n >= 2 && ((v.[0] = '"' && v.[n - 1] = '"') || (v.[0] = '\'' && v.[n - 1] = '\'')) then
      String.sub v 1 (n - 2)
    else v

  let parse_dotenv (src : string) : (string * string) list =
    String.split_on_char '\n' src
    |> List.filter_map (fun raw_line ->
         let line = String.trim raw_line in
         if line = "" || line.[0] = '#' then None
         else
           let line =
             if String.length line > 7 && String.sub line 0 7 = "export " then
               String.trim (String.sub line 7 (String.length line - 7))
             else line
           in
           match String.index_opt line '=' with
           | None -> None
           | Some i ->
             let k = String.trim (String.sub line 0 i) in
             let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
             if k = "" then None else Some (k, strip_quotes v))

  let starts_with ~prefix s =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix

  let filter_public ?(prefixes = [ "PUBLIC_"; "QUARTZ_PUBLIC_"; "VITE_" ]) vars =
    List.filter (fun (k, _) -> List.exists (fun prefix -> starts_with ~prefix k) prefixes) vars
end
