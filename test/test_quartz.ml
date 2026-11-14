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

let test_signals () =
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

  (* batch multiple updates so effect only fires once *)
  batch (fun () ->
    set_count 10;
    set_count 15;
    set_count 20);
  check "batch coalesced 3 sets into 1 run" (!runs = 3 && !last_seen = 80);

  dispose_effect eff;
  set_count 99;
  check "disposed effect stops firing" (!runs = 3);

  (* Time-travel undo/redo history *)
  let hist = make_history 10 in
  check "initial history value is 10" (history_get hist = 10 && hist.past = []);
  history_set hist 25;
  history_set hist 50;
  check "history updated to 50" (history_get hist = 50 && hist.past <> []);
  check "undo steps back to 25" (undo hist && history_get hist = 25 && hist.future <> []);
  check "redo steps forward to 50" (redo hist && history_get hist = 50)

let test_dom_and_styles () =
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
      ]
  in
  let compiled = compile_node view in
  check "HTML escapes <App>" (has_sub compiled.html "Quartz &lt;App&gt;");
  check "Dynamic rx_text rendered" (has_sub compiled.html "Count is 7");
  check "Hover CSS generated" (has_sub compiled.css ":hover{background-color:#1e293b}");
  check "Responsive @media generated" (has_sub compiled.css "@media(min-width:768px)");
  check "Click + mousemove handlers registered" (Hashtbl.length compiled.handlers = 2);

  (* Static DOM diagnostics *)
  let diag = Dom.diagnose view in
  check "DOM diagnostics counts interactive handlers" (diag.handler_count = 2 && diag.a11y_warnings = []);

  (* invoke registered handlers *)
  Hashtbl.iter
    (fun _ h ->
      match h with
      | On_click fn -> fn ()
      | On_mousemove fn -> fn (120, 340)
      | _ -> ())
    compiled.handlers;
  check "Registered click callback executed" !clicked;
  check "Registered mousemove callback executed" (!moved = (120, 340))

let test_router_form_components_tui () =
  (* 1. Router dynamic :param matching, layouts, query params & static link auditing *)
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
  let home_html = Dom.render_to_string (view ()) in
  check "Router home route & layout shell" (has_sub home_html "Home Page" && has_sub home_html "data-shell=\"1\"");

  let checked_links, broken_links = Router.audit_routes routes [ "/" ] view in
  check "Router static link auditor verifies valid parameterized links"
    (checked_links = [ "/users/42/tab/billing" ] && broken_links = []);

  Router.navigate "/users/42/tab/billing?filter=active";
  check "Router param + query_param extraction"
    (has_sub (Dom.render_to_string (view ())) "User=42 Tab=billing Filter=active");

  Router.set_query "filter" "archived";
  check "Router set_query updates reactive query state"
    (has_sub (Dom.render_to_string (view ())) "User=42 Tab=billing Filter=archived");

  Router.navigate "/nonexistent/route";
  check "Router 404 fallback" (has_sub (Dom.render_to_string (view ())) "404");

  (* 2. Form validation *)
  let email_f = Form.create_field Form.email in
  check "Untouched field has no visible error" (Form.error email_f = None);
  Form.set_value email_f "bad-email";
  check "Invalid email reports error" (Option.is_some (Form.error email_f));
  Form.set_value email_f "dev@ocaml.org";
  check "Valid email passes" (Form.error email_f = None && Form.validate email_f = Ok "dev@ocaml.org");

  (* 3. Rich SVG Charts & v2.0 UI Components *)
  let toggle_sig = use_signal true in
  let slider_sig = use_signal 75 in
  push_toast ~variant:Components.Success "Booted All systems nominal";
  let dash =
    spotlight_card
      ~title:"Revenue"
      [
        stat_card ~label:"MRR" ~value:"$42,500" ~delta:"+18.4%" ();
        sparkline [ 10.0; 15.0; 12.0; 22.0; 30.0 ];
        area_chart [ 20.0; 45.0; 35.0; 60.0; 80.0 ];
        donut_ring ~label:"CPU" ~pct:78.5 ();
        multi_line_chart [ ("A", "#38bdf8", [ 1.0; 2.0; 3.0 ]) ];
        heatmap ~cols:2 [ 10.0; 50.0; 80.0; 30.0 ];
        bar_chart [ ("Q1", 10.0); ("Q2", 25.0); ("Q3", 40.0) ];
        switch_toggle ~label:"Turbo" toggle_sig;
        slider ~label:"Cap" ~min:0 ~max:100 slider_sig;
        accordion [ ("faq1", "FAQ 1", text "Answer 1") ];
        toast_rack ();
      ]
  in
  let dash_html = Dom.render_to_string dash in
  check "Spotlight card attribute rendered" (has_sub dash_html "data-q-spotlight=\"1\"");
  check "Area chart linearGradient rendered" (has_sub dash_html "<linearGradient");
  check "Donut ring stroke-dasharray rendered" (has_sub dash_html "stroke-dasharray");
  check "Toast notification rendered" (has_sub dash_html "Booted");

  (* 4. Flexbox TUI Renderer *)
  let tui_str = Tui.render_tui ~width:60 ~height:12 dash in
  check "TUI frame contains title" (has_sub tui_str "Revenue");
  check "TUI frame contains value" (has_sub tui_str "$42,500");

  (* 5. Full LiveView Document, Event Dispatch & REST API *)
  api_get "/api/ping" (fun _ -> {|{"pong":true}|});
  let count_s, set_s = use_state 10 in
  let live_app () =
    div [
      rx_text (fun () -> Printf.sprintf "Val=%d" !~count_s);
      btn ~on_click:(fun () -> set_s (!~count_s + 5)) "Add5";
    ]
  in
  let doc, compiled = Server.render_full_document ~title:"TestLive" ~live:true (live_app ()) in
  check "Full HTML doc has DOCTYPE" (has_sub doc "<!DOCTYPE html>");
  check "Full HTML doc has Val=10" (has_sub doc "Val=10");
  Hashtbl.iter (fun id _ -> ignore (Server.dispatch_handler compiled.handlers ~id ~raw_val:"")) compiled.handlers;
  check "Dispatched server handler updated signal to 15" (!~count_s = 15)

let () =
  test_signals ();
  test_dom_and_styles ();
  test_router_form_components_tui ();
  Printf.printf "[OK] Quartz v2.0 (Fine-Grained Signals, Undo/Redo, SVG Charts, Spotlights, Multi-Page Router, Vite-Check & TUI) passed.\n%!"
