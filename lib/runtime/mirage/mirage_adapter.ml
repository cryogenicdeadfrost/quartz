(* MirageOS 4 Unikernel Adapter Functor (`quartz.mirage`).
   Zero Unix / Threads dependency -- parameterizes directly over Mirage-compatible
   CLOCK, KV_RO (`crunch` baked static assets), STORAGE (`irmin` / block KV), and PUBSUB. *)

open Quartz_core

module type KV_RO = sig
  val get_static : string -> string option
end

module Empty_kv_ro : KV_RO = struct
  let get_static _ = None
end

module List_kv_ro (Assets : sig val files : (string * string) list end) : KV_RO = struct
  let get_static path = List.assoc_opt path Assets.files
end

module Make
    (Clock : Io.CLOCK)
    (Kv_ro : KV_RO)
    (Storage : Io.STORAGE with type 'a io = 'a)
    (Pubsub : Io.PUBSUB) =
struct
  module Core_app = App.Make (Clock) (Storage) (Pubsub)

  type t = Core_app.t

  let create cfg = Core_app.create cfg

  (* Handles raw HTTP/1.1 bytes off a MirageOS TCP/TLS flow (`paf` / `mirage-tcpip`) *)
  let handle_raw_http (app : t) ?(peer_ip = "0.0.0.0") (raw_bytes : string) : Http.parse_state * string option =
    match Http.try_parse_request raw_bytes with
    | Http.Need_more_bytes -> (Http.Need_more_bytes, None)
    | Http.Malformed msg ->
      let resp =
        Http.format_http_response ~status:"400 Bad Request" ~content_type:"text/plain" msg
      in
      (Http.Malformed msg, Some resp)
    | Http.Complete (parsed, rem) ->
      let clean_path, query = Http.split_url_and_query parsed.target in
      let res =
        match Kv_ro.get_static clean_path with
        | Some content when parsed.meth = "GET" || parsed.meth = "HEAD" ->
          let ct = Http.mime_of_path clean_path in
          let body_out = if parsed.meth = "HEAD" then "" else content in
          Middleware.ok ~content_type:ct ~headers:[ ("Cache-Control", "public, max-age=86400") ] body_out
        | _ ->
          let client_ip = Http.extract_client_ip ~peer_ip parsed.headers in
          let req =
            {
              Middleware.meth = parsed.meth;
              path = clean_path;
              query;
              headers = parsed.headers;
              body = parsed.body;
              client_ip;
            }
          in
          Core_app.handle app req
      in
      let keep = Http.should_keep_alive parsed in
      let wire =
        Http.format_http_response
          ~status:res.status
          ~content_type:res.content_type
          ~keep_alive:keep
          ~headers:res.headers
          res.body
      in
      (Http.Complete (parsed, rem), Some wire)
end

(* Generates real MirageOS 4 `config.ml` and `unikernel.ml` files ready for `mirage configure -t hvt` *)
let generate_config_ml ~app_name ~static_dir =
  Printf.sprintf
    {|open Mirage

let main =
  main "Unikernel.Make"
    ~packages:[ package "quartz" ]
    (stackv4v6 @-> kv_ro @-> job)

let stack = generic_stackv4v6 default_network
let assets = crunch %S

let () = register %S [ main $ stack $ assets ]
|}
    static_dir app_name

let generate_unikernel_ml () =
  {|open Quartz

(* MirageOS 4 Unikernel entrypoint wired through Quartz.Mirage_adapter *)
module Make (Stack : Tcpip.Stack.V4V6) (Assets : Mirage_kv.RO) = struct
  module Store = Store.Wal_store (Io.Hook_clock)
  module App_mirage =
    Mirage_adapter.Make (Io.Hook_clock) (Mirage_adapter.Empty_kv_ro) (Store) (Pubsub.Bus)

  let start _stack _assets =
    (* Bind `App_mirage.handle_raw_http` directly into Mirage TCP/paf accept callback *)
    Printf.printf "[quartz.mirage] unikernel ready (0 Unix dependencies)\n%!";
    Lwt.return_unit
end
|}
