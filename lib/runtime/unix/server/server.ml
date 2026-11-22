(* Multi-Threaded HTTP/1.1 Keep-Alive + LiveView DOM-Diff + Per-Visitor Sessions +
   RFC 6455 WebSockets + Streaming SSE + Static /public Range Server. *)

open Quartz_core

type api_handler = (string * string) list -> string

let api_get_routes : (string, api_handler) Hashtbl.t = Hashtbl.create 16
let api_post_routes : (string, api_handler) Hashtbl.t = Hashtbl.create 16
let api_router : Middleware.Api_router.router = Middleware.Api_router.create ()
let global_middlewares : Middleware.t list ref = ref []
let readiness_check : (unit -> (unit, string) result) ref = ref (fun () -> Ok ())
let shutdown_hooks : (unit -> unit) list ref = ref []
let shutdown_requested : bool ref = ref false

let api_get path fn = Hashtbl.replace api_get_routes path fn
let api_post path fn = Hashtbl.replace api_post_routes path fn
let api_put ?middlewares path fn = Middleware.Api_router.put ?middlewares api_router path fn
let api_patch ?middlewares path fn = Middleware.Api_router.patch ?middlewares api_router path fn
let api_delete ?middlewares path fn = Middleware.Api_router.delete ?middlewares api_router path fn
let api_options ?middlewares path fn = Middleware.Api_router.options ?middlewares api_router path fn
let api_route ?middlewares ~meth path fn = Middleware.Api_router.add ?middlewares api_router ~meth ~pattern:path fn
let mount_router ?middlewares ~prefix sub = Middleware.Api_router.mount ?middlewares ~prefix api_router sub
let clear_api_router () = api_router.routes <- []; api_router.middlewares <- []
let use_middleware (mw : Middleware.t) = global_middlewares := !global_middlewares @ [ mw ]
let clear_middlewares () = global_middlewares := []
let get_api_routes () = api_get_routes
let set_readiness_check fn = readiness_check := fn
let on_shutdown fn = shutdown_hooks := fn :: !shutdown_hooks
let request_shutdown () = shutdown_requested := true

let client_runtime_js = Quartz_browser.Browser_runtime.client_runtime_js
let base_reset_css = App.base_reset_css

let render_full_document
    ?(title = "Quartz App")
    ?meta
    ?(live = true)
    ?(external_css_href = "")
    ?(external_js_src = "")
    (view : Dom.node) : string * Dom.compiled_view =
  App.render_full_document
    ~title
    ?meta
    ~live_script:client_runtime_js
    ~live
    ~external_css_href
    ~external_js_src
    view

let export_page
    ~path
    ?(title = "Quartz App")
    ?meta
    ?(external_css_href = "")
    ?(external_js_src = "")
    (app : unit -> Dom.node) =
  let doc, _ =
    render_full_document
      ~title
      ?meta
      ~live:false
      ~external_css_href
      ~external_js_src
      (app ())
  in
  let oc = open_out_bin path in
  output_string oc doc;
  close_out oc;
  String.length doc

let export_site ~out_dir ?(title = "Quartz Site") (pages : (string * string * (unit -> Dom.node)) list) =
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter
    (fun (filename, route_path, view_fn) ->
      Router.navigate route_path;
      let full_path = Filename.concat out_dir filename in
      ignore (export_page ~path:full_path ~title view_fn))
    pages

let write_all_substring fd s =
  let len = String.length s in
  let off = ref 0 in
  while !off < len do
    let wrote = Unix.write_substring fd s !off (len - !off) in
    if wrote <= 0 then off := len else off := !off + wrote
  done

let read_full_http_request fd (buf_ref : string ref) : Http.parsed_request option =
  let tmp = Bytes.create 8192 in
  let rec loop () =
    match Http.try_parse_request !buf_ref with
    | Http.Complete (req, rem) ->
      buf_ref := rem;
      Some req
    | Http.Malformed _ -> None
    | Http.Need_more_bytes ->
      let n = Unix.read fd tmp 0 8192 in
      if n <= 0 then None
      else (
        buf_ref := !buf_ref ^ Bytes.sub_string tmp 0 n;
        loop ()
      )
  in
  loop ()

let peer_ip_of_sockaddr = function
  | Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr
  | Unix.ADDR_UNIX s -> s

let serve_public_file ~public_dir req : Middleware.res option =
  if not (Sys.file_exists public_dir && Sys.is_directory public_dir) then None
  else
    let rel =
      if String.length req.Middleware.path > 0 && req.path.[0] = '/' then
        String.sub req.path 1 (String.length req.path - 1)
      else req.path
    in
    (* Prevent path traversal *)
    if String.contains rel '\000' || rel = "" then None
    else
      let segs = String.split_on_char '/' rel in
      if List.exists (fun s -> s = ".." || s = ".") segs then None
      else
        let disk_path =
          List.fold_left Filename.concat public_dir segs
        in
        if Sys.file_exists disk_path && not (Sys.is_directory disk_path) then
          let ic = open_in_bin disk_path in
          let content =
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () -> really_input_string ic (in_channel_length ic))
          in
          let ct = Http.mime_of_path disk_path in
          let total = String.length content in
          match Middleware.header_opt "range" req with
          | Some rng ->
            (match Http.parse_range ~total_len:total rng with
             | Some (s, e) ->
               let slice = String.sub content s (e - s + 1) in
               Some
                 {
                   Middleware.status = "206 Partial Content";
                   content_type = ct;
                   headers =
                     [
                       ("Accept-Ranges", "bytes");
                       ("Content-Range", Printf.sprintf "bytes %d-%d/%d" s e total);
                     ];
                   body = slice;
                 }
             | None -> Some (Middleware.reject ~status:"416 Range Not Satisfiable" "Bad byte range"))
          | None ->
            let out = if req.meth = "HEAD" then "" else content in
            let is_hashed_asset =
              String.length rel >= 7 && String.sub rel 0 7 = "assets/"
            in
            let cc =
              if is_hashed_asset then "public, max-age=31536000, immutable"
              else "public, max-age=3600"
            in
            Some
              (Middleware.ok ~content_type:ct
                 ~headers:[ ("Accept-Ranges", "bytes"); ("Cache-Control", cc) ]
                 out)
        else None

let handle_websocket_upgrade fd ~room ~sec_key ?on_message () =
  let handshake = Websocket.upgrade_response ~sec_websocket_key:sec_key in
  write_all_substring fd handshake;
  let conn_id = Websocket.Rooms.new_conn_id () in
  let send_mu = Mutex.create () in
  let send_raw bytes_s =
    Mutex.lock send_mu;
    Fun.protect ~finally:(fun () -> Mutex.unlock send_mu) (fun () -> write_all_substring fd bytes_s)
  in
  Websocket.Rooms.join ~room ~conn_id send_raw;
  let buf = ref "" in
  let tmp = Bytes.create 4096 in
  let alive = ref true in
  Fun.protect
    ~finally:(fun () -> Websocket.Rooms.leave_all ~conn_id)
    (fun () ->
      while !alive do
        match Websocket.decode_frame !buf with
        | Websocket.Protocol_error _ -> alive := false
        | Websocket.Frame (f, rem) ->
          buf := rem;
          (match f.opcode with
           | Websocket.Close ->
             (try send_raw (Websocket.close_frame ()) with _ -> ());
             alive := false
           | Websocket.Ping ->
             (try send_raw (Websocket.pong_frame f.payload) with _ -> alive := false)
           | Websocket.Text | Websocket.Binary ->
             (match on_message with
              | Some cb -> cb conn_id f.payload send_raw
              | None ->
                ignore (Websocket.Rooms.broadcast ~room ~except_conn_id:conn_id f.payload))
           | _ -> ())
        | Websocket.Need_more ->
          let n = try Unix.read fd tmp 0 4096 with _ -> 0 in
          if n <= 0 then alive := false
          else buf := !buf ^ Bytes.sub_string tmp 0 n
      done)

let handle_sse_stream ?(topic = "telemetry") fd =
  let hdr =
    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream; charset=utf-8\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n"
  in
  write_all_substring fd hdr;
  let recent =
    List.filter (fun (ev : Pubsub.event) -> topic = "*" || ev.topic = topic) (Pubsub.recent_events ())
  in
  let initial =
    String.concat "" (List.map Pubsub.format_sse_frame recent)
  in
  if initial <> "" then write_all_substring fd initial;
  let lock = Mutex.create () in
  let dead = ref false in
  let sub_id =
    Pubsub.subscribe_id ~topic (fun ev ->
      if not !dead then (
        Mutex.lock lock;
        Fun.protect
          ~finally:(fun () -> Mutex.unlock lock)
          (fun () ->
            try write_all_substring fd (Pubsub.format_sse_frame ev)
            with _ -> dead := true)
      ))
  in
  Fun.protect
    ~finally:(fun () -> Pubsub.unsubscribe sub_id)
    (fun () ->
      let tmp = Bytes.create 1 in
      try
        while not !dead do
          let r = Unix.select [ fd ] [] [] 1.0 in
          match r with
          | _ :: _, _, _ ->
            if Unix.read fd tmp 0 1 <= 0 then dead := true
          | _ ->
            Mutex.lock lock;
            Fun.protect
              ~finally:(fun () -> Mutex.unlock lock)
              (fun () ->
                try write_all_substring fd ": keepalive\n\n"
                with _ -> dead := true)
        done
      with _ -> ())

let run_browser
    ?(port = 8080)
    ?(title = "Quartz Studio Server")
    ?(public_dir = "public")
    ?(workers = 8)
    ?routes
    ?(redirects = [])
    ?(proxy = [])
    ?session_secret
    ?session_app
    ?max_requests
    (app : unit -> Dom.node) =
  shutdown_requested := false;
  if max_requests = None then (
    let handle_sig _ = shutdown_requested := true in
    (try Sys.set_signal Sys.sigint (Sys.Signal_handle handle_sig) with _ -> ());
    (try Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_sig) with _ -> ())
  );
  let secret =
    match session_secret with
    | Some s -> s
    | None -> Crypto_util.random_hex 32
  in
  let module Mem_store = Store.Wal_store (Unix_adapters.Unix_clock) in
  let module App_impl = App.Make (Unix_adapters.Unix_clock) (Mem_store) (Pubsub.Bus) in
  let legacy_get_apis =
    Hashtbl.fold
      (fun p fn acc ->
        (p, fun (r : Middleware.req) -> Middleware.ok (fn r.query)) :: acc)
      api_get_routes []
  in
  let legacy_post_apis =
    Hashtbl.fold
      (fun p fn acc ->
        (p, fun (r : Middleware.req) -> Middleware.ok (fn (Http.parse_form_urlencoded r.body))) :: acc)
      api_post_routes []
  in
  let effective_routes =
    match routes with
    | Some rs -> rs
    | None ->
      [
        Router.route ~title "/" (fun _ -> app ());
        Router.route ~title "/*" (fun _ -> app ());
      ]
  in
  let started_at = Unix.gettimeofday () in
  let total_requests = ref 0 in
  let inspector_handler (r : Middleware.req) : Middleware.res =
    let isr_st = Isr.get_stats () in
    let api_paths_str = Hashtbl.fold (fun k _ acc -> k :: acc) api_get_routes [] |> List.sort String.compare in
    let api_paths = List.map Json.string api_paths_str in
    let wants_html =
      match Http.header_opt "accept" r.headers with
      | Some a ->
        let la = String.lowercase_ascii a in
        let rec has s i =
          i + 9 <= String.length s && (String.sub s i 9 = "text/html" || has s (i + 1))
        in
        has la 0
      | None -> false
    in
    if wants_html then
      Middleware.html
        (Debug_registry.render_inspector_html
           ~title
           ~engine:"quartz-v4.2"
           ~uptime_s:(Unix.gettimeofday () -. started_at)
           ~requests_handled:!total_requests
           ~routes:effective_routes
           ~api_endpoints:api_paths_str
           ~recent_events:(Pubsub.recent_events ())
           ~access_logs:(Middleware.recent_access_logs ())
           ~registered_plugs:(Npm_plug.list_registered_plugs ()))
    else
      Middleware.ok
        (Json.to_pretty_string
           (Json.obj
              [
                ("engine", Json.string "quartz-v4");
                ("uptimeSec", Json.float (Unix.gettimeofday () -. started_at));
                ("requestsHandled", Json.int !total_requests);
                ("registeredRoutes", Json.int (List.length effective_routes));
                ("isrHits", Json.int isr_st.hits);
                ("isrMisses", Json.int isr_st.misses);
                ("apiGetEndpoints", Json.array api_paths);
                ("registeredPlugs", Json.array (List.map (fun (p, v) -> Json.string (p ^ "@" ^ v)) (Npm_plug.list_registered_plugs ())));
                ("recentEvents", Json.array (List.map (fun (ev : Pubsub.event) -> Json.obj [ ("id", Json.int ev.id); ("topic", Json.string ev.topic); ("payload", Json.string ev.payload) ]) (Pubsub.recent_events ())));
                ("accessLogs", Json.array (List.map Json.string (Middleware.recent_access_logs ())));
              ]))
  in
  let send_streaming_ssr client_fd ~title ?meta ~shell deferred_slots =
    let hdr = Http.format_streaming_headers ~status:"200 OK" ~content_type:"text/html" () in
    write_all_substring client_fd hdr;
    let chunks = App.render_streaming_document ~title ?meta ~shell deferred_slots in
    List.iter (fun ch -> write_all_substring client_fd (Http.encode_chunked_chunk ch)) chunks;
    write_all_substring client_fd Http.encode_chunked_finish
  in
  let app_instance =
    App_impl.create
      {
        App_impl.title;
        session_secret = secret;
        session_ttl_s = 86400;
        secure_cookies = false;
        client_script = client_runtime_js;
        routes = effective_routes;
        redirects;
        middlewares = !global_middlewares;
        rpcs = [];
        get_apis = ("/__quartz/inspector", inspector_handler) :: legacy_get_apis;
        post_apis = legacy_post_apis;
        static_assets = [];
        render_for_session =
          (fun sess (req : Middleware.req) ->
            Router.with_path req.path (fun () ->
              match session_app with
              | Some fn -> fn sess req
              | None -> app ()));
      }
  in
  let pool = Unix_adapters.Worker_pool.create ~workers () in
  let app_mu = Mutex.create () in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 128;
  Printf.printf "\n  QUARTZ v4.0 MULTI-THREADED HTTP/1.1 + WS + SSE + LIVEVIEW SERVER\n";
  Printf.printf "  -> Local:   http://localhost:%d/\n" port;
  Printf.printf "  -> Workers: %d OS threads | Keep-Alive | Per-Visitor Signed Cookies\n\n%!" workers;
  let accepted = ref 0 in
  let keep_accepting () =
    match max_requests with None -> true | Some m -> !accepted < m
  in
  let r_path_is_api_get p xs = List.mem_assoc p xs in
  let r_path_is_api_post p xs = List.mem_assoc p xs in
  let handle_client client_fd peer_addr =
    let peer_ip = peer_ip_of_sockaddr peer_addr in
    Fun.protect
      ~finally:(fun () -> try Unix.close client_fd with _ -> ())
      (fun () ->
        let buf = ref "" in
        let keep_conn = ref true in
        while !keep_conn do
          match read_full_http_request client_fd buf with
          | None -> keep_conn := false
          | Some parsed ->
            let clean_path, query = Http.split_url_and_query parsed.target in
            let is_ws =
              match Http.header_opt "upgrade" parsed.headers, Http.header_opt "sec-websocket-key" parsed.headers with
              | Some upg, Some key when String.lowercase_ascii (String.trim upg) = "websocket" ->
                Some key
              | _ -> None
            in
            match is_ws with
            | Some sec_key ->
              let room = if clean_path = "/__quartz_ws" then "__hmr" else clean_path in
              handle_websocket_upgrade client_fd ~room ~sec_key ();
              keep_conn := false
            | None ->
              if parsed.meth = "GET" && clean_path = "/__quartz_sse" then (
                let sse_topic = Option.value (List.assoc_opt "topic" query) ~default:"telemetry" in
                if max_requests <> None then (
                  let recent =
                    List.filter
                      (fun (ev : Pubsub.event) -> sse_topic = "*" || ev.topic = sse_topic)
                      (Pubsub.recent_events ())
                  in
                  let frames =
                    String.concat "" (List.map Pubsub.format_sse_frame recent)
                  in
                  let wire =
                    Http.format_http_response ~status:"200 OK" ~content_type:"text/event-stream"
                      ~keep_alive:false frames
                  in
                  write_all_substring client_fd wire
                ) else
                  handle_sse_stream ~topic:sse_topic client_fd;
                keep_conn := false
              ) else if parsed.meth = "GET"
                        && (clean_path = "/__quartz_stream" || List.assoc_opt "stream" query = Some "1") then (
                incr total_requests;
                let shell_node =
                  Mutex.lock app_mu;
                  Fun.protect
                    ~finally:(fun () -> Mutex.unlock app_mu)
                    (fun () ->
                      let target_p = if clean_path = "/__quartz_stream" then "/" else clean_path in
                      Router.with_path target_p (fun () -> app ()))
                in
                send_streaming_ssr client_fd ~title ~shell:shell_node [];
                keep_conn := false
              ) else (
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
                let resp =
                  match serve_public_file ~public_dir req with
                  | Some static_res -> static_res
                  | None ->
                    incr total_requests;
                    Mutex.lock app_mu;
                    Fun.protect
                      ~finally:(fun () -> Mutex.unlock app_mu)
                      (fun () ->
                        (* Sync any dynamically added routes into global app config cleanly *)
                        let current_apis_g =
                          Hashtbl.fold
                            (fun p fn acc ->
                              (p, fun (r : Middleware.req) -> Middleware.ok (fn r.query)) :: acc)
                            api_get_routes []
                        in
                        let current_apis_p =
                          Hashtbl.fold
                            (fun p fn acc ->
                              (p, fun (r : Middleware.req) -> Middleware.ok (fn (Http.parse_form_urlencoded r.body))) :: acc)
                            api_post_routes []
                        in
                        let try_proxy () =
                          List.find_map
                            (fun (prefix, target_base) ->
                              let lp = String.length prefix in
                              if lp > 0 && String.length req.path >= lp && String.sub req.path 0 lp = prefix then
                                let base_clean =
                                  let lb = String.length target_base in
                                  if lb > 0 && target_base.[lb - 1] = '/' then String.sub target_base 0 (lb - 1)
                                  else target_base
                                in
                                let qs = Es_std.Search_params.to_string req.query in
                                let full_url = base_clean ^ req.path ^ (if qs = "" then "" else "?" ^ qs) in
                                let fwd_hdrs =
                                  List.filter
                                    (fun (k, _) ->
                                      let lk = String.lowercase_ascii k in
                                      lk <> "host" && lk <> "connection" && lk <> "content-length")
                                    req.headers
                                in
                                match Fetch.request ~meth:req.meth ~headers:fwd_hdrs ~body:req.body full_url with
                                | Ok r ->
                                  let ct = Option.value ~default:"application/octet-stream" (List.assoc_opt "content-type" r.headers) in
                                  let st = Printf.sprintf "%d %s" r.status (if r.status_text = "" then "OK" else r.status_text) in
                                  Some (Middleware.status_res ~status:st ~content_type:ct r.body)
                                | Error err ->
                                  Some (Middleware.reject ~status:"502 Bad Gateway" ("Proxy error: " ^ err))
                              else None)
                            proxy
                        in
                        if req.meth = "GET" && req.path = "/healthz" then
                          Middleware.json_res (Json.obj [ ("ok", Json.bool true); ("status", Json.string "alive") ])
                        else if req.meth = "GET" && req.path = "/readyz" then
                          (match !readiness_check () with
                           | Ok () -> Middleware.json_res (Json.obj [ ("ok", Json.bool true); ("status", Json.string "ready") ])
                           | Error reason ->
                             Middleware.json_res ~status:"503 Service Unavailable"
                               (Json.obj [ ("ok", Json.bool false); ("error", Json.string reason) ]))
                        else
                          match try_proxy () with
                          | Some proxied -> proxied
                          | None ->
                            if req.meth = "GET" && r_path_is_api_get req.path current_apis_g then
                              let wrapped = Middleware.pipeline !global_middlewares (List.assoc req.path current_apis_g) in
                              wrapped req
                            else if req.meth = "POST" && r_path_is_api_post req.path current_apis_p then
                              let wrapped = Middleware.pipeline !global_middlewares (List.assoc req.path current_apis_p) in
                              wrapped req
                            else
                              match Middleware.Api_router.dispatch_opt api_router req with
                              | Some res ->
                                let wrapped = Middleware.pipeline !global_middlewares (fun _ -> res) in
                                wrapped req
                              | None ->
                                if req.meth = "OPTIONS" then
                                  Middleware.status_res ~status:"204 No Content"
                                    ~headers:
                                      [
                                        ("Access-Control-Allow-Origin", "*");
                                        ("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS");
                                        ("Access-Control-Allow-Headers", "Content-Type, Authorization, X-CSRF-Token");
                                      ]
                                    ""
                                else
                                  App_impl.handle app_instance req)
                in
                let ka = Http.should_keep_alive parsed && max_requests = None in
                let wire =
                  Http.format_http_response
                    ~status:resp.status
                    ~content_type:resp.content_type
                    ~keep_alive:ka
                    ~headers:resp.headers
                    resp.body
                in
                write_all_substring client_fd wire;
                if not ka then keep_conn := false
              )
        done)
  in
  while keep_accepting () && not !shutdown_requested do
    let ready =
      if max_requests <> None then true
      else
        try
          let r, _, _ = Unix.select [ sock ] [] [] 0.25 in
          r <> []
        with Unix.Unix_error (Unix.EINTR, _, _) -> false
    in
    if ready then (
      try
        let client_fd, peer_addr = Unix.accept sock in
        incr accepted;
        if max_requests <> None then
          handle_client client_fd peer_addr
        else
          Unix_adapters.Worker_pool.submit pool (fun () -> handle_client client_fd peer_addr)
      with Unix.Unix_error (Unix.EINTR, _, _) -> ()
    )
  done;
  Unix_adapters.Worker_pool.shutdown pool;
  List.iter (fun fn -> try fn () with _ -> ()) !shutdown_hooks;
  (try Unix.close sock with _ -> ())
