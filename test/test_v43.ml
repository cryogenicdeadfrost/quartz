(* Comprehensive integration tests for Quartz v4.3.0:
   ES2025/2026 primitives, React/Solid reactive tools, Express REST routing,
   Node Env/Emitter/Path, real loopback Fetch + Proxy, and Vite asset minification. *)

open Quartz

let checks = ref 0

let check msg cond =
  incr checks;
  if not cond then (
    Printf.eprintf "[FAIL] %s\n%!" msg;
    exit 1
  )

let test_promises_abort_timers_scope () =
  (* Promise.with_resolvers + let* + catch + finally *)
  let r = Promise.with_resolvers () in
  let ran_finally = ref false in
  let chained =
    let open Promise in
    let* x = r.promise in
    let+ y = resolve (x * 2) in
    y + 1
  in
  let guarded = Promise.finally chained (fun () -> ran_finally := true) in
  check "promise initially pending" (Promise.state guarded = Promise.Pending);
  r.resolve 20;
  check "promise resolved through let*/let+" (Promise.state guarded = Promise.Fulfilled 41);
  check "finally callback fired" !ran_finally;

  let caught =
    Promise.try_ (fun () -> failwith "kaboom")
    |> fun p -> Promise.catch p (fun _ -> Promise.resolve 99)
  in
  check "Promise.try_ + catch recovers" (Promise.state caught = Promise.Fulfilled 99);

  let all_p = Promise.all [ Promise.resolve 1; Promise.resolve 2; Promise.resolve 3 ] in
  check "Promise.all preserves order" (Promise.state all_p = Promise.Fulfilled [ 1; 2; 3 ]);

  let any_p = Promise.any [ Promise.reject "e1"; Promise.resolve "win"; Promise.reject "e2" ] in
  check "Promise.any picks first fulfilled" (Promise.state any_p = Promise.Fulfilled "win");

  let settled = Promise.all_settled [ Promise.resolve 7; Promise.reject "nope" ] in
  check "Promise.all_settled captures both"
    (Promise.state settled = Promise.Fulfilled [ Promise.Ok_settled 7; Promise.Err_settled "nope" ]);

  (* Timers + debounce + throttle + AbortController.timeout *)
  Timers.clear_all ();
  let fake_now = ref 100.0 in
  let clock () = !fake_now in
  let debounced_val = ref "" in
  let on_type = Timers.debounce ~clock ~ms:100 (fun s -> debounced_val := s) in
  on_type "q";
  fake_now := 100.05;
  on_type "qu";
  ignore (Timers.tick ~clock ());
  check "debounce holds before window" (!debounced_val = "");
  fake_now := 100.20;
  ignore (Timers.tick ~clock ());
  check "debounce fires latest value" (!debounced_val = "qu");

  let th_count = ref 0 in
  let ping = Timers.throttle ~clock ~ms:500 (fun () -> incr th_count) in
  ping ();
  fake_now := 100.30;
  ping ();
  fake_now := 100.80;
  ping ();
  check "throttle drops intermediate call" (!th_count = 2);

  let sig_t = Abort.timeout ~clock ~ms:200 () in
  check "timeout signal not aborted yet" (not (Abort.is_aborted sig_t));
  fake_now := 101.05;
  ignore (Timers.tick ~clock ());
  check "timeout signal aborted after window" (Abort.is_aborted sig_t);

  let c1 = Abort.create_controller () in
  let c2 = Abort.create_controller () in
  let combined = Abort.any [ Abort.signal c1; Abort.signal c2 ] in
  Abort.abort ~reason:"user_cancel" c2;
  check "Abort.any propagates abort reason" (Abort.reason combined = Some "user_cancel");

  (* Scope (ES2026 DisposableStack) *)
  let log = ref [] in
  Scope.with_scope (fun sc ->
    Scope.defer sc (fun () -> log := "first" :: !log);
    Scope.defer sc (fun () -> log := "second" :: !log));
  check "Scope disposes LIFO" (!log = [ "first"; "second" ])

let test_signals_context_dom_primitives () =
  let cleanups = ref 0 in
  let n = use_signal 1 in
  let h =
    use_effect (fun () ->
      let _ = !~n in
      on_cleanup (fun () -> incr cleanups))
  in
  n <~ 2;
  Signal.dispose_effect h;
  check "on_cleanup runs on re-run and dispose" (!cleanups = 2);

  let nums = use_signal [ 1; 2; 3; 4; 5; 6 ] in
  let evens = Signal.filter (fun x -> x mod 2 = 0) nums in
  let sum_evens = Signal.reduce ( + ) 0 evens in
  let grouped = Signal.group_by (fun x -> if x mod 2 = 0 then "even" else "odd") nums in
  check "Signal.filter + reduce" (!~sum_evens = 12);
  check "Signal.group_by buckets" (List.length !~grouped = 2);

  (* Persisted signal *)
  let backing = Hashtbl.create 4 in
  let theme =
    use_persisted ~key:"theme"
      ~encode:(fun s -> s)
      ~decode:(fun s -> Some s)
      ~load:(Hashtbl.find_opt backing)
      ~save:(Hashtbl.replace backing)
      "dark"
  in
  theme <~ "solar";
  check "use_persisted saves updates" (Hashtbl.find_opt backing "theme" = Some "solar");

  (* React-style Context *)
  let theme_ctx = create_context "light" in
  check "Context default outside provider" (use_context theme_ctx = "light");
  let inside =
    provide_context theme_ctx "obsidian" (fun () ->
      let nested = provide_context theme_ctx "neon" (fun () -> use_context theme_ctx) in
      (nested, use_context theme_ctx))
  in
  check "Context nests and unwinds cleanly" (inside = ("neon", "obsidian"));
  check "Context restored after provide" (use_context theme_ctx = "light");

  (* Error boundary, Portal, Lazy load, Keyed for_each, Refs & querySelector *)
  let safe =
    error_boundary
      ~fallback:(fun err -> div ~class_:"err-box" [ text ("Caught: " ^ err) ])
      (fun () -> failwith "render_crash")
  in
  let safe_html = render_html safe in
  check "error_boundary isolates crash" (String.length safe_html > 10 && Option.is_some (query_selector ".err-box" safe));

  let btn_ref = use_ref ~id:"cta-btn" () in
  let tree =
    div ~id:"app" [
      nav [
        a ~href:"/docs" ~class_:"nav-link active" [ text "Docs" ];
        a ~href:"/api" ~class_:"nav-link" [ text "API" ];
      ];
      attach_ref btn_ref (button [ text "Ship" ]);
      for_each ~key:string_of_int ~items:(fun () -> [ 10; 20 ]) (fun x -> li [ textf "%d" x ]);
      portal ~target:"#modal-root" [ div ~class_:"modal-body" [ text "Hi" ] ];
      lazy_load ~fallback:(fun () -> span [ text "Wait..." ]) (fun () -> p [ text "Loaded!" ]);
    ]
  in
  check "attach_ref binds ref.current" (Option.is_some btn_ref.current);
  check "query_selector descendant nav a.active" (Option.is_some (query_selector "nav a.active" tree));
  check "query_selector #id" (Option.is_some (query_selector "#cta-btn" tree));
  check "query_selector [attr=val]" (Option.is_some (query_selector "[data-q-key=20]" tree));
  check "query_selector_all .nav-link count = 2" (List.length (query_selector_all ".nav-link" tree) = 2);
  let html = render_html tree in
  check "portal emits data-q-portal" (Option.is_some (query_selector "[data-q-portal=#modal-root]" tree));
  check "lazy_load emits data-q-lazy template" (String.length html > 100 && Option.is_some (query_selector "[data-q-lazy=1]" tree));

  let got = ref "" in
  let lid = add_event_listener ~event:"toast" (fun d -> got := d) in
  let count = dispatch_event ~event:"toast" ~detail:"saved" () in
  remove_event_listener ~event:"toast" lid;
  check "DOM custom event bus dispatches" (count = 1 && !got = "saved")

let test_es_std_primitives () =
  (* Url + Search_params *)
  let u = Result.get_ok (Url.parse "https://example.com:8443/docs/intro?tag=ocaml&tag=web#top") in
  check "Url host and port" (u.host = "example.com" && u.port = Some 8443 && u.hash = Some "top");
  check "Search_params.get_all" (Search_params.get_all "tag" u.query = [ "ocaml"; "web" ]);
  let resolved = Result.get_ok (Url.resolve ~base:u "../api/v1?x=a%20b") in
  check "Url.resolve normalizes .." (resolved.path = "/api/v1" && Search_params.get "x" resolved.query = Some "a b");

  (* ES2025 Iter helpers *)
  let pipe =
    Iter.of_list [ 1; 2; 3; 4; 5; 6; 7 ]
    |> Iter.filter (fun x -> x mod 2 = 1)
    |> Iter.map (fun x -> x * 10)
    |> Iter.drop 1
    |> Iter.take 2
    |> Iter.to_list
  in
  check "Iter lazy pipeline" (pipe = [ 30; 50 ]);
  check "Iter.chunk" (Iter.to_list (Iter.chunk 2 (Iter.of_list [ 1; 2; 3; 4; 5 ])) = [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ]);

  (* ES2025 Set_ops + Regex_util.escape *)
  check "Set_ops.intersection" (Set_ops.intersection [ 1; 2; 3 ] [ 2; 3; 4 ] = [ 2; 3 ]);
  check "Set_ops.symmetric_difference" (Set_ops.symmetric_difference [ 1; 2 ] [ 2; 3 ] = [ 1; 3 ]);
  check "Set_ops.is_subset_of" (Set_ops.is_subset_of [ 1; 2 ] [ 1; 2; 3 ]);
  check "Regex_util.escape" (Regex_util.escape "a+b*(c)?" = {|a\+b\*\(c\)\?|});

  (* ES2026 Temporal *)
  let d1 = Result.get_ok (Temporal.Plain_date.of_iso "2026-11-22") in
  let d2 = Temporal.Plain_date.add_days d1 10 in
  check "Temporal.Plain_date add/between"
    (Temporal.Plain_date.to_iso d2 = "2026-12-02" && Temporal.Plain_date.days_between d1 d2 = 10);
  let dur = Temporal.Duration.make ~hours:2 ~minutes:30 () in
  let i0 = Temporal.Instant.of_epoch_ms 0L in
  let i1 = Temporal.Instant.add i0 dur in
  check "Temporal.Instant add/since" (Temporal.Duration.to_ms (Temporal.Instant.since ~other:i0 i1) = 9_000_000L);

  (* Node Emitter + Path + Env *)
  let em = Emitter.create () in
  let hits = ref 0 in
  let id1 = Emitter.on em "ping" (fun n -> hits := !hits + n) in
  ignore (Emitter.once em "ping" (fun n -> hits := !hits + n * 10));
  Emitter.emit em "ping" 2;
  Emitter.emit em "ping" 2;
  Emitter.off em "ping" id1;
  Emitter.emit em "ping" 5;
  check "Emitter on/once/off" (!hits = 24);

  check "Path.join normalizes" (Path.join [ "/var/www"; "site/../assets"; "app.js" ] = "/var/www/assets/app.js");
  check "Path.extname" (Path.extname "/a/b/style.min.css" = ".css");
  check "Path.relative" (Path.relative ~from:"/a/b/c" ~to_:"/a/d/e.ml" = "../../d/e.ml");

  Env.clear ();
  ignore (Env.load_string "SECRET_KEY=topsecret\nPUBLIC_APP_NAME=\"Quartz Studio\"\nPORT=4040\nDEBUG=true\n");
  check "Env.int & Env.bool" (Env.int "PORT" = 4040 && Env.bool "DEBUG");
  let pub = Env.public_vars () in
  check "Env.public_vars hides server secrets"
    (List.length pub = 1 && List.assoc_opt "PUBLIC_APP_NAME" pub = Some "Quartz Studio")

let test_router_base_path_and_api_router () =
  (* Router base_path + with_path restoration *)
  Router.set_base_path "/quartz-pages/";
  check "Router.base_path normalized" (Router.base_path () = "/quartz-pages");
  check "Router.href prepends base" (Router.href "/docs" = "/quartz-pages/docs");
  Router.navigate "/quartz-pages/components";
  check "Router.navigate strips base" (Signal.peek (Router.current_path ()) = "/components");
  let inner =
    Router.with_path "/quartz-pages/backend" (fun () -> Signal.peek (Router.current_path ()))
  in
  check "Router.with_path sets and restores path"
    (inner = "/backend" && Signal.peek (Router.current_path ()) = "/components");
  Router.set_base_path "";

  (* Express-style Api_router + error_handler *)
  let users = Api_router.create () in
  Api_router.get users "/boom" (fun _ _ -> failwith "unexpected_db_crash");
  Api_router.get users "/:id" (fun params _req ->
    let id = Option.value ~default:"" (List.assoc_opt "id" params) in
    Middleware.json_res (Json.obj [ ("userId", Json.string id) ]));
  Api_router.put users "/:id" (fun params req ->
    let id = Option.value ~default:"" (List.assoc_opt "id" params) in
    Middleware.json_res ~status:"200 OK" (Json.obj [ ("updated", Json.string id); ("body", Json.string req.body) ]));
  Api_router.delete users "/:id" (fun _ _ -> Middleware.no_content ());

  let root_api = Api_router.create () in
  Api_router.use root_api (Middleware.error_handler ~include_trace:true ());
  Api_router.mount ~prefix:"/api/v1/users" root_api users;
  let handler = Api_router.to_handler root_api in

  let mk_req meth path body =
    { Middleware.meth; path; query = []; headers = []; body; client_ip = "127.0.0.1" }
  in
  let r_get = handler (mk_req "GET" "/api/v1/users/42" "") in
  check "Api_router GET :id" (r_get.status = "200 OK" && r_get.body = {|{"userId":"42"}|});
  let r_del = handler (mk_req "DELETE" "/api/v1/users/42" "") in
  check "Api_router DELETE 204" (r_del.status = "204 No Content");
  let r_opt = handler (mk_req "OPTIONS" "/api/v1/users/42" "") in
  check "Api_router OPTIONS preflight" (r_opt.status = "204 No Content");
  let r_err = handler (mk_req "GET" "/api/v1/users/boom" "") in
  check "Middleware.error_handler catches exception" (r_err.status = "500 Internal Server Error")

let test_live_server_fetch_and_proxy () =
  (* Spin up two short-lived loopback servers in background threads:
     backend on 19431, frontend with ~proxy:[("/upstream", "http://127.0.0.1:19431")] on 19432 *)
  Server.clear_api_router ();
  Server.set_readiness_check (fun () -> Ok ());
  api_put "/api/items/:id" (fun params req ->
    let id = Option.value ~default:"" (List.assoc_opt "id" params) in
    Middleware.created (Json.obj [ ("id", Json.string id); ("payload", Json.string req.body) ])
    |> Middleware.with_cookie ~name:"q_sid" ~value:"abc123");
  api_route ~meth:"GET" "/upstream/ping" (fun _ _ ->
    Middleware.json_res (Json.obj [ ("pong", Json.bool true) ]));

  let t_back =
    Thread.create
      (fun () ->
        run_browser ~port:19431 ~workers:2 ~max_requests:1 (fun () -> div [ text "back" ]))
      ()
  in
  let t_front =
    Thread.create
      (fun () ->
        run_browser ~port:19432 ~workers:2 ~max_requests:4
          ~proxy:[ ("/upstream", "http://127.0.0.1:19431") ]
          (fun () -> div [ text "front" ]))
      ()
  in
  Unix.sleepf 0.15;

  let h_res = Result.get_ok (Fetch.get "http://127.0.0.1:19432/healthz") in
  check "Live /healthz returns 200" (h_res.status = 200 && h_res.ok);

  let r_res = Result.get_ok (Fetch.get "http://127.0.0.1:19432/readyz") in
  check "Live /readyz returns 200" (r_res.status = 200 && r_res.ok);

  let put_res = Result.get_ok (Fetch.request ~meth:"PUT" ~body:"hello" "http://127.0.0.1:19432/api/items/99") in
  check "Live PUT /api/items/:id returns 201 + cookie"
    (put_res.status = 201 && List.mem_assoc "set-cookie" put_res.headers);

  let px_res = Result.get_ok (Fetch.get "http://127.0.0.1:19432/upstream/ping") in
  check "Live dev proxy forwards /upstream/ping" (px_res.status = 200 && px_res.body = {|{"pong":true}|});

  Thread.join t_front;
  Thread.join t_back;

  (* Aborted fetch fails immediately without touching network *)
  let ac = Abort.create_controller () in
  Abort.abort ~reason:"cancelled_before_send" ac;
  let aborted = Fetch.get ~abort:(Abort.signal ac) "http://127.0.0.1:19432/healthz" in
  check "Fetch honors AbortController" (aborted = Error "cancelled_before_send")

let test_vite_minifier_and_manifest () =
  let css = "/* banner */\nbody {\n  color:  #fff;\n}\n" in
  let min_css = Vite.minify_css css in
  check "Vite.minify_css strips comments/newlines" (min_css = "body { color: #fff; }");

  let js = "// comment line\n  const x = 1;\n\n  // another\n  return x;\n" in
  let min_js = Vite.minify_js js in
  check "Vite.minify_js strips comment lines" (min_js = "const x = 1;\nreturn x;")

let () =
  test_promises_abort_timers_scope ();
  test_signals_context_dom_primitives ();
  test_es_std_primitives ();
  test_router_base_path_and_api_router ();
  test_live_server_fetch_and_proxy ();
  test_vite_minifier_and_manifest ();
  Printf.printf "[OK] Quartz v4.3.0 ES2025/2026 + Express/Node/Vite Suite: %d assertions passed!\n%!" !checks
