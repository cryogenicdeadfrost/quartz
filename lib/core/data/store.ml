(* Pure-OCaml Embedded Database Engine (`Quartz.Store`):
   - Append-only Write-Ahead Log (WAL) + In-memory Index + Snapshot/Compaction
   - Implements Io.STORAGE (runs in-memory, persists via WAL snapshot serializer or Unix WAL)
   - Typed JSON Document Collection & Schema Migrations (`Migrations.run`) *)

module Wal_store (C : Io.CLOCK) = struct
  type 'a io = 'a

  type record_entry = {
    value : string;
    expires_at : float option;
  }

  type op =
    | Op_set of string * string * float option
    | Op_del of string
    | Op_log of string * string

  type t = {
    kv : (string, record_entry) Hashtbl.t;
    logs : (string, string list) Hashtbl.t;
    mutable wal : op list;
  }

  let create () =
    { kv = Hashtbl.create 64; logs = Hashtbl.create 16; wal = [] }

  let global_default = create ()

  let encode_op = function
    | Op_set (k, v, exp) ->
      let exp_j = match exp with None -> Json.null | Some f -> Json.float f in
      Json.to_string (Json.obj [ ("op", Json.string "set"); ("k", Json.string k); ("v", Json.string v); ("exp", exp_j) ])
    | Op_del k ->
      Json.to_string (Json.obj [ ("op", Json.string "del"); ("k", Json.string k) ])
    | Op_log (topic, entry) ->
      Json.to_string (Json.obj [ ("op", Json.string "log"); ("t", Json.string topic); ("e", Json.string entry) ])

  let apply_op t now = function
    | Op_set (k, v, expires_at) ->
      (match expires_at with
       | Some exp when now > exp -> Hashtbl.remove t.kv k
       | _ -> Hashtbl.replace t.kv k { value = v; expires_at })
    | Op_del k -> Hashtbl.remove t.kv k
    | Op_log (topic, entry) ->
      let cur = Option.value ~default:[] (Hashtbl.find_opt t.logs topic) in
      Hashtbl.replace t.logs topic (cur @ [ entry ])

  let replay_wal_string t raw =
    let now = C.now_s () in
    let lines = String.split_on_char '\n' raw in
    let count = ref 0 in
    List.iter
      (fun l ->
        let tl = String.trim l in
        if tl <> "" then
          match Json.parse tl with
          | Ok (Json.Object _ as j) ->
            let open Json in
            (match Option.bind (get_field "op" j) get_string with
             | Some "set" ->
               (match Option.bind (get_field "k" j) get_string, Option.bind (get_field "v" j) get_string with
                | Some k, Some v ->
                  let exp = Option.bind (get_field "exp" j) get_float in
                  let op = Op_set (k, v, exp) in
                  apply_op t now op;
                  t.wal <- op :: t.wal;
                  incr count
                | _ -> ())
             | Some "del" ->
               (match Option.bind (get_field "k" j) get_string with
                | Some k ->
                  let op = Op_del k in
                  apply_op t now op;
                  t.wal <- op :: t.wal;
                  incr count
                | None -> ())
             | Some "log" ->
               (match Option.bind (get_field "t" j) get_string, Option.bind (get_field "e" j) get_string with
                | Some topic, Some e ->
                  let op = Op_log (topic, e) in
                  apply_op t now op;
                  t.wal <- op :: t.wal;
                  incr count
                | _ -> ())
             | _ -> ())
          | _ -> ())
      lines;
    !count

  let dump_snapshot t : string =
    let now = C.now_s () in
    let lines = ref [] in
    Hashtbl.iter
      (fun k item ->
        match item.expires_at with
        | Some exp when now > exp -> ()
        | exp -> lines := encode_op (Op_set (k, item.value, exp)) :: !lines)
      t.kv;
    Hashtbl.iter
      (fun topic entries ->
        List.iter (fun e -> lines := encode_op (Op_log (topic, e)) :: !lines) entries)
      t.logs;
    String.concat "\n" (List.rev !lines)

  let compact t : int =
    let now = C.now_s () in
    let expired = ref [] in
    Hashtbl.iter
      (fun k item ->
        match item.expires_at with
        | Some exp when now > exp -> expired := k :: !expired
        | _ -> ())
      t.kv;
    List.iter (Hashtbl.remove t.kv) !expired;
    let new_wal = ref [] in
    Hashtbl.iter (fun k item -> new_wal := Op_set (k, item.value, item.expires_at) :: !new_wal) t.kv;
    Hashtbl.iter (fun tp es -> List.iter (fun e -> new_wal := Op_log (tp, e) :: !new_wal) es) t.logs;
    t.wal <- !new_wal;
    List.length !new_wal

  let get_on t key =
    match Hashtbl.find_opt t.kv key with
    | None -> None
    | Some item ->
      (match item.expires_at with
       | Some exp when C.now_s () > exp ->
         Hashtbl.remove t.kv key;
         None
       | _ -> Some item.value)

  let set_on t ?ttl_s key value =
    let expires_at = Option.map (fun s -> C.now_s () +. float_of_int s) ttl_s in
    let op = Op_set (key, value, expires_at) in
    Hashtbl.replace t.kv key { value; expires_at };
    t.wal <- op :: t.wal

  let delete_on t key =
    Hashtbl.remove t.kv key;
    t.wal <- Op_del key :: t.wal

  let list_prefix_on t prefix =
    let now = C.now_s () in
    let plen = String.length prefix in
    let acc = ref [] in
    Hashtbl.iter
      (fun k item ->
        let alive = match item.expires_at with Some e -> now <= e | None -> true in
        if alive && String.length k >= plen && String.sub k 0 plen = prefix then
          acc := (k, item.value) :: !acc)
      t.kv;
    List.sort (fun (a, _) (b, _) -> String.compare a b) !acc

  let get key = get_on global_default key
  let set ?ttl_s key v = set_on global_default ?ttl_s key v
  let delete key = delete_on global_default key
  let list_prefix p = list_prefix_on global_default p
  let append_log ~topic entry =
    let op = Op_log (topic, entry) in
    apply_op global_default (C.now_s ()) op;
    global_default.wal <- op :: global_default.wal
  let read_log ~topic =
    Option.value ~default:[] (Hashtbl.find_opt global_default.logs topic)
end

(* Idempotent Schema Migration Runner over any STORAGE *)
module Migrations (S : Io.STORAGE with type 'a io = 'a) = struct
  type migration = {
    version : int;
    name : string;
    up : unit -> (unit, string) result;
  }

  let run (ms : migration list) : (int, string) result =
    let sorted = List.sort (fun a b -> compare a.version b.version) ms in
    let applied = ref 0 in
    let rec loop = function
      | [] -> Ok !applied
      | m :: rest ->
        let key = Printf.sprintf "__migration:%04d:%s" m.version m.name in
        (match S.get key with
         | Some _ -> loop rest
         | None ->
           (match m.up () with
            | Error e -> Error (Printf.sprintf "Migration %d (%s) failed: %s" m.version m.name e)
            | Ok () ->
              S.set key "applied";
              incr applied;
              loop rest))
    in
    loop sorted
end
