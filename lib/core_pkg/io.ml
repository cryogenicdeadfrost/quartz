(* OS-independent runtime interfaces and hooks for Quartz Core.
   Zero dependency on Unix, Threads, Mirage, or Js_of_ocaml. *)

module type IO = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Sync_io : IO with type 'a t = 'a = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let map f x = f x
end

module type CLOCK = sig
  val now_s : unit -> float
  val now_us : unit -> int64
end

(* Global clock hook used by backward-compatible v3 helpers.
   `quartz.unix` installs `Unix.gettimeofday` at startup; Mirage/Edge/Browser
   install their native monotonic/wall clock. Default fallback steps cleanly
   so even pure-core unit tests work deterministically without Unix. *)
module Clock_hook = struct
  let fallback_counter = ref 1700000000.0
  let hook_fn : (unit -> float) ref =
    ref (fun () ->
      fallback_counter := !fallback_counter +. 0.0001;
      !fallback_counter)

  let set fn = hook_fn := fn
  let now_s () = !hook_fn ()
  let now_us () = Int64.of_float (now_s () *. 1_000_000.0)
end

module Memory_clock = struct
  let current = ref 1700000000.0
  let set t = current := t
  let advance dt = current := !current +. dt
  let now_s () = !current
  let now_us () = Int64.of_float (!current *. 1_000_000.0)
end

module Hook_clock : CLOCK = struct
  let now_s = Clock_hook.now_s
  let now_us = Clock_hook.now_us
end

module type STORAGE = sig
  type 'a io
  val get : string -> string option io
  val set : ?ttl_s:int -> string -> string -> unit io
  val delete : string -> unit io
  val list_prefix : string -> (string * string) list io
  val append_log : topic:string -> string -> unit io
  val read_log : topic:string -> string list io
end

module type SYNC_STORAGE = STORAGE with type 'a io = 'a

module Memory_storage (C : CLOCK) : sig
  include STORAGE with type 'a io = 'a
  val clear : unit -> unit
  val size : unit -> int
  val prune_expired : unit -> int
end = struct
  type 'a io = 'a
  type item = {
    value : string;
    expires_at : float option;
  }

  let kv : (string, item) Hashtbl.t = Hashtbl.create 64
  let logs : (string, string list) Hashtbl.t = Hashtbl.create 16

  let clear () =
    Hashtbl.clear kv;
    Hashtbl.clear logs

  let is_expired now item =
    match item.expires_at with
    | None -> false
    | Some exp -> now > exp

  let prune_expired () =
    let now = C.now_s () in
    let expired = ref [] in
    Hashtbl.iter
      (fun k v -> if is_expired now v then expired := k :: !expired)
      kv;
    List.iter (Hashtbl.remove kv) !expired;
    List.length !expired

  let size () =
    ignore (prune_expired ());
    Hashtbl.length kv

  let get key =
    match Hashtbl.find_opt kv key with
    | None -> None
    | Some item ->
      if is_expired (C.now_s ()) item then (
        Hashtbl.remove kv key;
        None
      ) else Some item.value

  let set ?ttl_s key value =
    let expires_at =
      match ttl_s with
      | None -> None
      | Some s -> Some (C.now_s () +. float_of_int s)
    in
    Hashtbl.replace kv key { value; expires_at }

  let delete key = Hashtbl.remove kv key

  let list_prefix prefix =
    let now = C.now_s () in
    let plen = String.length prefix in
    let acc = ref [] in
    Hashtbl.iter
      (fun k item ->
        if (not (is_expired now item))
           && String.length k >= plen
           && String.sub k 0 plen = prefix
        then acc := (k, item.value) :: !acc)
      kv;
    List.sort (fun (a, _) (b, _) -> String.compare a b) !acc

  let append_log ~topic entry =
    let cur = Option.value ~default:[] (Hashtbl.find_opt logs topic) in
    Hashtbl.replace logs topic (cur @ [ entry ])

  let read_log ~topic =
    Option.value ~default:[] (Hashtbl.find_opt logs topic)
end

module type PUBSUB = sig
  val publish : topic:string -> string -> unit
  val subscribe : topic:string -> (string -> unit) -> int
  val unsubscribe : int -> unit
end

module Memory_pubsub : PUBSUB = struct
  let next_id = ref 1
  let subs : (string, (int * (string -> unit)) list) Hashtbl.t = Hashtbl.create 16

  let subscribe ~topic fn =
    let id = !next_id in
    incr next_id;
    let cur = Option.value ~default:[] (Hashtbl.find_opt subs topic) in
    Hashtbl.replace subs topic ((id, fn) :: cur);
    id

  let unsubscribe id =
    Hashtbl.iter
      (fun topic lst ->
        let filtered = List.filter (fun (sid, _) -> sid <> id) lst in
        Hashtbl.replace subs topic filtered)
      subs

  let publish ~topic msg =
    match Hashtbl.find_opt subs topic with
    | None -> ()
    | Some lst -> List.iter (fun (_, fn) -> fn msg) (List.rev lst)
end

module type LOGGER = sig
  val info : string -> unit
  val warn : string -> unit
  val error : string -> unit
end

module Stdout_logger : LOGGER = struct
  let info msg = Printf.printf "[INFO] %s\n%!" msg
  let warn msg = Printf.printf "[WARN] %s\n%!" msg
  let error msg = Printf.eprintf "[ERROR] %s\n%!" msg
end

module Null_logger : LOGGER = struct
  let info _ = ()
  let warn _ = ()
  let error _ = ()
end

module type SIGNER = sig
  val sign : secret:string -> payload:string -> string
  val verify : secret:string -> payload:string -> sig_hex:string -> bool
end
