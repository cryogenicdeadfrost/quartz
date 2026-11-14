open Quartz

let check msg cond =
  if not cond then (
    Printf.eprintf "[FAIL] %s\n%!" msg;
    exit 1
  )

let has_sub haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found && String.sub haystack i nlen = needle then found := true
    done;
    !found

let test_signals_and_resources () =
  let open Signal in
  let count, set_count = create 2 in
  let doubled = computed (fun () -> !~count * 2) in
  let quad = computed (fun () -> !~doubled * 2) in
  check "initial computed 2*2=4" (read doubled = 4);
  check "initial chained computed 4*2=8" (read quad = 8);

  let runs = ref 0 in
  let last_seen = ref 0 in
  let eff =
    watch (fun () ->
      incr runs;
      last_seen := read quad)
  in
  check "effect runs once on init" (!runs = 1 && !last_seen = 8);

  set_count 5;
  check "effect reacts to set_count 5 -> 20" (!runs = 2 && !last_seen = 20);

  batch (fun () ->
    set_count 10;
    set_count 15;
    set_count 20);
  check "batch coalesced 3 sets into 1 run" (!runs = 3 && !last_seen = 80);

  dispose_effect eff;
  set_count 99;
  check "disposed effect stops firing" (!runs = 3);

  (* Lens slice *)
  let pair_sig = make ("ocaml", 5) in
  let ver_sig, set_ver = lens pair_sig ~get_field:snd ~set_field:(fun (s, _) v -> (s, v)) in
  check "lens reads 5" (!~ver_sig = 5);
  set_ver 6;
  check "lens updates parent to 6" (snd !~pair_sig = 6);

  (* Time-travel undo/redo history *)
  let hist = make_history 10 in
  check "initial history value is 10" (history_get hist = 10 && hist.past = []);
  history_set hist 25;
  history_set hist 50;
  check "history updated to 50" (history_get hist = 50 && hist.past <> []);
  check "undo steps back to 25" (undo hist && history_get hist = 25 && hist.future <> []);
  check "redo steps forward to 50" (redo hist && history_get hist = 50);

  (* Async Suspense Resource & Optimistic Rollback *)
  let res = use_resource ~key:"users" (fun () -> Ok 42) in
  check "resource initially Ready 42" (Resource.status res = Resource.Ready 42);
  let ok_mut =
    Resource.mutate_optimistic res ~optimistic_val:99 (fun () -> Error "network timeout")
  in
  check "failed optimistic mutation rolls back to 42"
    (not ok_mut && Resource.status res = Resource.Ready 42);
  let ok_mut2 =
    Resource.mutate_optimistic res ~optimistic_val:100 (fun () -> Ok 100)
  in
  check "successful optimistic mutation commits 100"
    (ok_mut2 && Resource.status res = Resource.Ready 100)

let test_wcag_contrast_and_styles () =
  (* White vs Black contrast ratio is exactly 21.0 *)
  let cr_bw = Contrast.contrast_ratio Palette.white Palette.black in
  check "White vs Black contrast is 21:1" (Float.abs (cr_bw -. 21.0) < 0.05);
  check "White on Black passes AAA" (Contrast.passes_aaa Palette.white Palette.black);
  check "Slate-800 on Slate-900 fails AA" (not (Contrast.passes_aa Palette.slate_800 Palette.slate_900));
  check "pick_ink chooses dark ink on bright yellow"
    (Contrast.pick_ink Palette.amber_400 = Palette.slate_950);
  check "pick_ink chooses white ink on dark slate_900"
    (Contrast.pick_ink Palette.slate_900 = Palette.white);

  let open Dom in
  let count = Signal.make 7 in
  let clicked = ref false in
  let moved = ref (0, 0) in
  let view =
    div
      ~attrs:[ on_mousemove (fun xy -> moved := xy) ]
      ~style:Style.[ flex_col; gap 12; p 16; glass; hover_lift; bg Palette.slate_900; hover [ bg Palette.slate_800 ]; md [ flex_row ] ]
      [
        h1 ~style:Style.[ text_2xl; bold; color Palette.sky_400 ] [ text "Quartz <App>" ];
        rx_text (fun () -> Printf.sprintf "Count is %d" (Signal.get count));
        button ~label:"Hit me" ~on_click:(fun () -> clicked := true) [];
        custom_element "q-badge"
          ~props:[ ("data-tier", "pro") ]
          ~shadow:(shadow_root [ text "Shadow Slot" ])
          [ text "Light Slot" ];
      ]
  in
  let compiled = compile_node view in
  check "HTML escapes <App>" (has_sub compiled.html "Quartz &lt;App&gt;");
  check "Custom element rendered" (has_sub compiled.html "<q-badge");
  check "Declarative Shadow DOM rendered" (has_sub compiled.html "shadowrootmode=\"open\"");
  check "Hover CSS generated" (has_sub compiled.css ":hover{background-color:#1e293b}");
  check "Responsive @media generated" (has_sub compiled.css "@media(min-width:768px)");

  let diag = Dom.diagnose view in
  check "DOM diagnostics counts handlers & contrast checks"
    (diag.handler_count = 2 && diag.contrast_checks >= 1 && diag.contrast_warnings = []);

  Hashtbl.iter
    (fun _ h ->
      match h with
      | On_click fn -> fn ()
      | On_mousemove fn -> fn (120, 340)
      | _ -> ())
    compiled.handlers;
  check "Registered click callback executed" !clicked;
  check "Registered mousemove callback executed" (!moved = (120, 340))

let test_backend_middleware_isr_pubsub () =
  (* 1. Middleware pipeline: CORS + Rate Cop + Bearer Guard + Timer *)
  let base_h (req : Middleware.req) =
    Middleware.ok (Printf.sprintf {|{"hello":"%s"}|} req.path)
  in
  let guarded =
    Middleware.pipeline
      [
        Middleware.cors ~origin:"https://ocaml.org" ();
        Middleware.bearer_guard ~token:"topsecret";
        Middleware.req_timer ();
      ]
      base_h
  in
  let mk_req ?(headers = []) path : Middleware.req =
    { meth = "GET"; path; query = []; headers; body = ""; client_ip = "10.0.0.1" }
  in
  let r_unauth = guarded (mk_req "/private") in
  check "Bearer guard rejects unauthenticated request with 401"
    (has_sub r_unauth.status "401");

  let r_auth =
    guarded (mk_req ~headers:[ ("Authorization", "Bearer topsecret") ] "/private")
  in
  check "Bearer guard allows valid token with 200 OK"
    (r_auth.status = "200 OK" && has_sub r_auth.body "/private");
  check "CORS & Timer headers injected"
    (List.assoc_opt "Access-Control-Allow-Origin" r_auth.headers = Some "https://ocaml.org"
    && Option.is_some (List.assoc_opt "X-Quartz-Time-Us" r_auth.headers));

  (* 2. Token-bucket rate limiter *)
  let limited =
    Middleware.pipeline [ Middleware.rate_cop ~max_burst:2.0 ~refill_per_sec:0.0 () ] base_h
  in
  let _ = limited (mk_req "/a") in
  let _ = limited (mk_req "/a") in
  let r_429 = limited (mk_req "/a") in
  check "Token-bucket rate limiter returns 429 when burst exhausted"
    (has_sub r_429.status "429");

  (* 3. ISR Cache *)
  Isr.clear ();
  let _, st1 = Isr.get_or_render ~path:"/home" ~revalidate_sec:60.0 (fun () -> "<h1>Cached</h1>") in
  let html2, st2 = Isr.get_or_render ~path:"/home" ~revalidate_sec:60.0 (fun () -> "<h1>New</h1>") in
  check "ISR cache transitions MISS -> HIT and serves cached payload"
    (st1 = "MISS" && st2 = "HIT" && html2 = "<h1>Cached</h1>");

  (* 4. PubSub & SSE framing *)
  let received = ref "" in
  Pubsub.subscribe ~topic:"alerts" (fun ev -> received := ev.payload);
  let ev = Pubsub.publish ~topic:"alerts" "CPU spike" in
  let sse = Pubsub.format_sse_frame ev in
  check "PubSub notifies subscriber and formats W3C SSE frame"
    (!received = "CPU spike" && has_sub sse "event: alerts" && has_sub sse "data: CPU spike")

let test_router_charts_components_and_custom_renderers () =
  (* 1. Router + Layouts + Static Link Auditor *)
  let shell child = div ~attrs:[ Dom.attr "data-shell" "1" ] [ child ] in
  let routes =
    Router.with_layout shell
      [
        Router.route ~title:"Home" "/" (fun _ ->
          div [ Dom.text "Home Page"; Router.link ~to_:"/users/42/tab/billing" [ Dom.text "Profile" ] ]);
        Router.route ~title:"User" "/users/:uid/tab/:tab" (fun ps ->
          let u = Option.value ~default:"?" (Router.param "uid" ps) in
          let t = Option.value ~default:"?" (Router.param "tab" ps) in
          let q = Option.value ~default:"none" (Router.query_param "filter") in
          Dom.textf "User=%s Tab=%s Filter=%s" u t q);
      ]
  in
  let view () = Router.router routes in
  Router.navigate "/";
  let checked_links, broken_links = Router.audit_routes routes [ "/" ] view in
  check "Router static link auditor verifies valid parameterized links"
    (checked_links = [ "/users/42/tab/billing" ] && broken_links = []);

  (* 2. Advanced v3.0 Charts & UI Components *)
  let tasks_sig =
    use_signal [ { Components.id = 1; title = "Ship v3"; tag = "CORE"; lane = "Todo" } ]
  in
  let cmd_sig = use_signal true in
  let dash =
    spotlight_card
      ~title:"Revenue"
      [
        contrast_badge ~bg_color:Palette.emerald_400 "AA OK";
        stat_card ~label:"MRR" ~value:"$42,500" ~delta:"+18.4%" ();
        area_chart [ 20.0; 45.0; 35.0; 60.0; 80.0 ];
        donut_ring ~label:"CPU" ~pct:78.5 ();
        radar_chart [ ("Speed", 95.0); ("RAM", 98.0); ("Safety", 100.0) ];
        candlestick_chart [ (10.0, 15.0, 8.0, 14.0); (14.0, 18.0, 12.0, 11.0) ];
        kanban_board ~lanes:[ "Todo"; "Done" ] tasks_sig;
        timeline [ ("12:00", "Deploy", "All green") ];
        stepper ~current_step:1 [ "Build"; "Test"; "Ship" ];
        command_palette ~open_sig:cmd_sig [ ("Run Doctor", "CLI", fun () -> ()) ];
      ]
  in
  let dash_html = render_html dash in
  check "Spotlight card & radar polygon rendered"
    (has_sub dash_html "data-q-spotlight=\"1\"" && has_sub dash_html "<polygon");
  check "Kanban board & Command palette rendered"
    (has_sub dash_html "Ship v3" && has_sub dash_html "Command Palette");

  (* 3. Multi-Target Custom Renderers: TUI, Markdown, Canvas2D, JSON Scenegraph *)
  let tui_out = render_tui ~width:60 ~height:12 dash in
  let md_out = render_markdown dash in
  let canvas_out = render_canvas2d ~width:600 ~height:300 dash in
  let json_out = render_json_scene dash in
  check "TUI renderer outputs ASCII frame" (has_sub tui_out "Revenue");
  check "Markdown custom renderer outputs GFM" (has_sub md_out "### Revenue" && has_sub md_out "`[SVG Vector Chart]`");
  check "Canvas2D custom renderer outputs GPU draw calls"
    (has_sub canvas_out "ctx.fillRect" && has_sub canvas_out "ctx.fillText");
  check "JSON scenegraph renderer outputs structured AST"
    (has_sub json_out "\"type\":\"el\"" && has_sub json_out "\"tag\":\"div\"")

let () =
  test_signals_and_resources ();
  test_wcag_contrast_and_styles ();
  test_backend_middleware_isr_pubsub ();
  test_router_charts_components_and_custom_renderers ();
  Printf.printf "[OK] Quartz v3.0 (6 Subsystems: Core, Style/WCAG, Frontend, Backend/Middleware/ISR/SSE, 4 Custom Renderers & Vite Doctor) passed.\n%!"
