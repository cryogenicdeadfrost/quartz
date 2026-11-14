open Quartz

type deployment = {
  service : string;
  region : string;
  rps : string;
  latency : string;
  healthy : bool;
}

type event_log = {
  id : int;
  level : string;
  msg : string;
  ms : float;
}

(* Register Express/NestJS-style Middlewares & JSON REST APIs *)
let setup_backend () =
  Server.clear_middlewares ();
  use_middleware (Middleware.cors ());
  use_middleware (Middleware.security_headers ());
  use_middleware (Middleware.req_timer ());
  use_middleware (Middleware.rate_cop ~max_burst:120.0 ~refill_per_sec:30.0 ());

  api_get "/api/health" (fun _params ->
    {|{"status":"ok","engine":"quartz-v3","subsystems":["core","style","frontend","backend","renderers","tooling"]}|});
  api_get "/api/metrics" (fun _params ->
    let isr_st = Isr.get_stats () in
    Printf.sprintf
      {|{"rps":194800,"p99_us":380,"isr_hits":%d,"isr_misses":%d}|}
      isr_st.hits isr_st.misses);
  api_post "/api/deploy" (fun form_params ->
    ignore (publish_event ~topic:"deploy" "New microservice provisioned via REST API");
    Printf.sprintf {|{"deployed":true,"fields":%d}|} (List.length form_params))

let deploys, set_deploys =
  use_state
    [
      { service = "edge-auth-proxy"; region = "eu-central-1"; rps = "34,800"; latency = "0.8 ms"; healthy = true };
      { service = "realtime-stream-core"; region = "us-east-1"; rps = "142,500"; latency = "0.3 ms"; healthy = true };
      { service = "quartz-ssr-worker"; region = "us-west-2"; rps = "29,300"; latency = "1.1 ms"; healthy = true };
      { service = "webhook-dispatcher"; region = "ap-northeast-1"; rps = "6,410"; latency = "14.2 ms"; healthy = false };
    ]

(* Reactive cluster knobs & computed signals *)
let replicas = use_signal 4
let cpu_target = use_signal 68
let auto_heal = use_signal true
let turbo_cache = use_signal true
let est_cost = use_memo (fun () -> !~replicas * 45 + (if !~turbo_cache then 25 else 0))
let total_cap = use_memo (fun () -> !~replicas * 65_000)

(* Time-travel undo/redo state & Async Suspense Resource *)
let counter_hist = use_history 10
let cluster_resource =
  use_resource ~key:"cluster-health-check" (fun () ->
    Ok "All 38 Global Edge POPs Synchronized (0 packet loss)")

(* Interactive Kanban board state *)
let kanban_tasks =
  use_signal
    [
      { Components.id = 1; title = "Zero-copy SSE frame broadcaster"; tag = "BACKEND"; lane = "Backlog" };
      { Components.id = 2; title = "WCAG 2.1 relative luminance engine"; tag = "STYLE"; lane = "In Progress" };
      { Components.id = 3; title = "Canvas2D & Markdown custom renderers"; tag = "RENDERER"; lane = "Done" };
    ]

let throughput_series, set_throughput =
  use_state [ 112.0; 124.0; 119.0; 138.0; 152.0; 149.0; 168.0; 174.0; 169.0; 184.2 ]

let events, set_events =
  use_state
    [
      { id = 204; level = "INFO"; msg = "Scaled us-east-1 shard group to 4 replicas"; ms = 0.42 };
      { id = 203; level = "SYNC"; msg = "Flushed edge CDN cache across 38 POPs"; ms = 1.15 };
      { id = 202; level = "WARN"; msg = "Elevated p99 jitter detected in ap-northeast-1"; ms = 14.20 };
    ]

let next_eid, set_eid = use_state 205

let push_telemetry_spike delta label_msg =
  let cur = !~throughput_series in
  let last_v = List.nth cur (List.length cur - 1) in
  let next_v = Float.max 40.0 (last_v +. delta) in
  let trimmed =
    match cur with
    | _ :: rest -> rest @ [ next_v ]
    | [] -> [ next_v ]
  in
  let eid = !~next_eid in
  batch (fun () ->
    set_throughput trimmed;
    set_eid (eid + 1);
    set_events
      ({ id = eid; level = (if delta >= 0.0 then "BURST" else "DROP"); msg = label_msg; ms = Float.abs delta *. 0.08 }
      :: List.filteri (fun i _ -> i < 5) !~events));
  ignore (publish_event ~topic:"telemetry" label_msg);
  push_toast
    ~variant:(if delta >= 0.0 then Components.Success else Components.Warning)
    label_msg

(* Overlays: Modal, Slide-over Drawer & Ctrl+K Command Palette *)
let modal_open = use_signal false
let drawer_open = use_signal false
let cmd_open = use_signal false
let cursor_coords, set_cursor = use_state (240, 120)

let email_field = Form.create_field ~initial:"platform@ocaml.org" Form.email
let service_field = Form.create_field ~initial:"vector-indexer" (Form.min_len 3)

(* Shared Application Shell Layout *)
let app_shell child_view =
  div
    ~style:Style.[ h_screen; bg Palette.slate_950; color Palette.slate_100 ]
    [
      navbar
        ~brand:"Quartz v3.0"
        ~right:
          (div ~style:Style.[ flex_row; items_center; gap 10 ] [
             avatar ~initials:"AB" ~online:true ();
             btn ~variant:Primary ~on_click:(fun () -> cmd_open <~ true) "Ctrl+K Actions";
             btn ~variant:Neutral ~on_click:(fun () -> drawer_open <~ true) "Inspector";
           ])
        [
          ("/", "Overview");
          ("/analytics", "Telemetry & Charts");
          ("/components", "Kanban & Lab");
          ("/network", "Middleware & Renderers");
        ];
      div
        ~style:Style.[ max_w 1200; mx_auto; px 28; pt 16 ]
        [
          div
            ~style:Style.[ flex_row; justify_between; items_center ]
            [
              breadcrumbs ();
              rx (fun () ->
                span ~style:Style.[ text_xs; font_mono; color Palette.slate_400 ]
                  [ textf "Active Shards: %d nodes | Est. Burn: $%d/mo" !~replicas !~est_cost ]);
            ];
        ];
      child_view;
      toast_rack ();
      command_palette
        ~open_sig:cmd_open
        [
          ("Trigger +25k rps Traffic Burst", "Telemetry", fun () -> push_telemetry_spike 25.0 "Ctrl+K Burst +25k rps");
          ("Navigate to Telemetry & Charts", "Route /analytics", fun () -> navigate "/analytics");
          ("Navigate to Kanban & Reactive Lab", "Route /components", fun () -> navigate "/components");
          ("Toggle Zero-Copy Turbo Cache", "Signal Flip", fun () -> turbo_cache <~ not !~turbo_cache);
        ];
      drawer
        ~open_sig:drawer_open
        ~title:"Live Runtime, WCAG & Signal Inspector"
        [
          alert ~variant:Primary ~title:"Modular Subsystem Architecture"
            "Split cleanly into core/, style/, frontend/, backend/, renderers/, and tooling/.";
          rx (fun () ->
            div ~style:Style.[ flex_col; gap 12 ] [
              progress_bar ~label:"Cluster CPU Utilization" ~pct:(float_of_int !~cpu_target) ();
              progress_bar ~color:Palette.emerald_500 ~label:"Memory Headroom" ~pct:(if !~turbo_cache then 88.0 else 54.0) ();
            ]);
          switch_toggle ~label:"Autonomous Self-Healing" auto_heal;
          switch_toggle ~label:"Zero-Copy Turbo Cache (+$25/mo)" turbo_cache;
          code_block ~lang:"ocaml"
            "module Custom = Custom_renderer.Make(My_Backend)\nlet md = Quartz.render_markdown (studio_app ())";
        ];
    ]

(* PAGE 1: Overview Control Plane *)
let overview_page _ =
  div
    ~style:Style.[ flex_col; gap 24; p 28; max_w 1200; mx_auto; animate_fade_in ]
    [
      spotlight_card
        [
          div
            ~style:Style.[ flex_row; justify_between; items_center ]
            [
              div
                ~style:Style.[ flex_col; gap 8 ]
                [
                  div ~style:Style.[ flex_row; items_center; gap 10 ] [
                    badge ~variant:Primary "OCAML 5 FULL-STACK";
                    contrast_badge ~bg_color:Palette.emerald_400 "WCAG AA VERIFIED";
                    contrast_badge ~bg_color:Palette.sky_400 "4 CUSTOM RENDERERS";
                  ];
                  h1 ~style:Style.[ text_4xl; bold; m 0; color Palette.white ]
                    [ text "Quartz v3.0 Reactive Platform & Multi-Renderer" ];
                  p ~style:Style.[ text_base; color Palette.slate_300; m 0 ]
                    [ text "Featuring fine-grained Signals, WCAG 2.1 luminance math, Express/Nest-style Middleware, ISR, SSE, and Canvas2D/Markdown/TUI/HTML renderers." ];
                ];
              div
                ~style:Style.[ flex_col; gap 10; items_end ]
                [
                  btn ~variant:Primary ~on_click:(fun () -> modal_open <~ true) "+ Deploy Service";
                  btn ~variant:Success
                    ~on_click:(fun () -> push_telemetry_spike 18.5 "Ingested +18.5k rps edge burst")
                    "Trigger Traffic Burst";
                ];
            ];
        ];

      (* Suspense Resource Banner *)
      suspense ~resource:cluster_resource (fun status_msg ->
        div
          ~style:
            Style.[
              flex_row;
              justify_between;
              items_center;
              px 18;
              py 10;
              rounded_xl;
              bg Palette.slate_900;
              border ~width:1 ~color:Palette.emerald_500 ();
            ]
          [
            span ~style:Style.[ text_xs; font_mono; color Palette.emerald_400 ]
              [ text ("[OK] [Suspense Resource Ready] " ^ status_msg) ];
            stepper ~current_step:2 [ "Compile AST"; "Audit WCAG & Links"; "LiveView / Edge SSR" ];
          ]);

      (* 4 Interactive Spotlight KPI Cards *)
      div
        ~style:Style.[ grid_cols 4; gap 16 ]
        [
          rx (fun () ->
            let cur = !~throughput_series in
            let last_v = List.nth cur (List.length cur - 1) in
            stat_card
              ~label:"Global Edge Throughput"
              ~value:(Printf.sprintf "%.1fk rps" last_v)
              ~delta:"+14.2% vs 1h ago"
              ());
          rx (fun () ->
            stat_card
              ~label:"Provisioned Capacity"
              ~value:(Printf.sprintf "%dk rps" (!~total_cap / 1000))
              ~delta:(Printf.sprintf "%d Worker Shards" !~replicas)
              ());
          rx (fun () ->
            stat_card
              ~label:"P99 Wire Latency"
              ~value:(if !~turbo_cache then "380 us" else "920 us")
              ~delta:(if !~turbo_cache then "Turbo Cache ON" else "Standard I/O")
              ());
          rx (fun () ->
            stat_card
              ~label:"Projected Monthly Burn"
              ~value:(Printf.sprintf "$%d/mo" !~est_cost)
              ~delta:(if !~auto_heal then "Auto-Heal Active" else "Manual Ops")
              ());
        ];

      (* 2-Column Area Chart + Donut Gauges *)
      div
        ~style:Style.[ grid_cols 2; gap 20 ]
        [
          spotlight_card
            ~title:"Real-Time Global Ingestion (Area SVG)"
            ~subtitle:"Pure OCaml SVG linear gradient & coordinate compiler"
            ~right:(badge ~variant:Success "Live Stream")
            [
              rx (fun () ->
                area_chart ~width:520 ~height:165 ~color:"#38bdf8" !~throughput_series);
              div
                ~style:Style.[ flex_row; gap 10; mt 14 ]
                [
                  btn ~variant:Success
                    ~on_click:(fun () -> push_telemetry_spike 14.0 "Edge burst +14k rps in eu-central-1")
                    "+ Spike (+14k rps)";
                  btn ~variant:Danger
                    ~on_click:(fun () -> push_telemetry_spike (-11.5) "Shed 11.5k rps background batch")
                    "- Shed Load (-11.5k rps)";
                  btn ~variant:Neutral
                    ~on_click:(fun () -> cmd_open <~ true)
                    "Open Ctrl+K Palette";
                ];
            ];
          spotlight_card
            ~title:"Cluster Resource Rings & Live Controls"
            ~subtitle:"Drag slider or flip switches to recompute SVG stroke-dashoffset"
            [
              rx (fun () ->
                div
                  ~style:Style.[ flex_row; justify_between; items_center; py 8 ]
                  [
                    donut_ring ~size:115 ~color:"#38bdf8" ~label:"CPU Target"
                      ~pct:(float_of_int !~cpu_target) ();
                    donut_ring ~size:115 ~color:"#10b981" ~label:"Cache Hit"
                      ~pct:(if !~turbo_cache then 96.4 else 74.0) ();
                    donut_ring ~size:115 ~color:"#8b5cf6" ~label:"Shard Load"
                      ~pct:(Float.min 100.0 (float_of_int (!~replicas * 18))) ();
                  ]);
              div
                ~style:Style.[ flex_col; gap 12; mt 10 ]
                [
                  slider ~label:"Target CPU Utilization Ceiling (%)" ~min:20 ~max:95 cpu_target;
                  div ~style:Style.[ flex_row; justify_between; items_center ] [
                    switch_toggle ~label:"Zero-Copy Turbo Cache" turbo_cache;
                    div ~style:Style.[ flex_row; gap 8 ] [
                      btn ~variant:Neutral ~on_click:(fun () -> replicas <~ max 1 (!~replicas - 1)) "- Shard";
                      btn ~variant:Primary ~on_click:(fun () -> replicas <~ !~replicas + 1) "+ Shard";
                    ];
                  ];
                ];
            ];
        ];

      (* Fleet Table + Live Event Log Tabs *)
      spotlight_card
        ~title:"Global Microservice Fleet & Event Stream"
        ~subtitle:"Click any service link to test parameterized routing (/service/:id)"
        ~right:(btn ~variant:Primary ~on_click:(fun () -> modal_open <~ true) "+ Add Microservice")
        [
          tabs
            ~initial:"fleet"
            [
              ( "fleet",
                "Production Services",
                fun () ->
                  rx (fun () ->
                    data_table
                      ~columns:
                        [
                          ( "Service",
                            fun d ->
                              link ~to_:("/service/" ^ d.service)
                                ~style:Style.[ color Palette.sky_400; bold ]
                                [ text d.service ] );
                          ("Region", fun d -> code [ text d.region ]);
                          ("Throughput", fun d -> text (d.rps ^ " req/s"));
                          ("Latency", fun d -> text d.latency);
                          ( "Status",
                            fun d ->
                              if d.healthy then badge ~variant:Success "Healthy"
                              else badge ~variant:Danger "Degraded" );
                        ]
                      ~rows:!~deploys) );
              ( "events",
                "Real-Time Audit Log",
                fun () ->
                  rx (fun () ->
                    data_table
                      ~columns:
                        [
                          ("Event ID", fun e -> code [ textf "#%d" e.id ]);
                          ( "Tier",
                            fun e ->
                              if e.level = "WARN" || e.level = "DROP" then badge ~variant:Warning e.level
                              else badge ~variant:Primary e.level );
                          ("Summary", fun e -> text e.msg);
                          ("Delta", fun e -> textf "%.2f ms" e.ms);
                        ]
                      ~rows:!~events) );
            ];
        ];

      modal
        ~open_sig:modal_open
        ~title:"Deploy New Native Microservice"
        [
          div
            ~style:Style.[ flex_col; gap 14 ]
            [
              Form.view_input ~label:"Service Identifier (min 3 chars)" service_field;
              Form.view_input ~label:"On-Call Engineer Email" email_field;
              div
                ~style:Style.[ flex_row; justify_end; gap 10; mt 12 ]
                [
                  btn ~variant:Neutral ~on_click:(fun () -> modal_open <~ false) "Cancel";
                  btn ~variant:Success
                    ~on_click:(fun () ->
                      match Form.validate service_field, Form.validate email_field with
                      | Ok srv, Ok _ ->
                        set_deploys
                          (!~deploys
                          @ [ { service = srv; region = "eu-west-1"; rps = "18,400"; latency = "0.6 ms"; healthy = true } ]);
                        modal_open <~ false;
                        push_toast ~variant:Components.Success ("Deployed " ^ srv ^ " to eu-west-1")
                      | _ -> ())
                    "Deploy Now";
                ];
            ];
        ];
    ]

(* PAGE 2: Multi-Series Analytics, Radar, Candlestick & Heatmaps *)
let analytics_page _ =
  let selected_region = Option.value ~default:"all" (query_param "region") in
  div
    ~style:Style.[ flex_col; gap 24; p 28; max_w 1200; mx_auto; animate_fade_in ]
    [
      div
        ~style:Style.[ flex_row; justify_between; items_center ]
        [
          div ~style:Style.[ flex_col; gap 4 ] [
            h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Vector Telemetry: Multi-Line, Radar, OHLC & Heatmap" ];
            p ~style:Style.[ text_sm; color Palette.slate_400; m 0 ]
              [ textf "Active Query Filter: ?region=%s (reactive URL query state)" selected_region ];
          ];
          div ~style:Style.[ flex_row; gap 8 ] [
            btn ~variant:(if selected_region = "all" then Primary else Neutral)
              ~on_click:(fun () -> set_query "region" "all") "All Regions";
            btn ~variant:(if selected_region = "us-east" then Primary else Neutral)
              ~on_click:(fun () -> set_query "region" "us-east") "US-East";
            btn ~variant:(if selected_region = "eu-central" then Primary else Neutral)
              ~on_click:(fun () -> set_query "region" "eu-central") "EU-Central";
          ];
        ];
      div
        ~style:Style.[ grid_cols 2; gap 20 ]
        [
          spotlight_card
            ~title:"Latency Percentile Curves (P50 / P95 / P99)"
            ~subtitle:"Multi-series vector chart with auto-scaled axes"
            [
              multi_line_chart ~width:520 ~height:170
                [
                  ("P50 (us)", "#10b981", [ 210.; 205.; 218.; 195.; 190.; 188.; 182. ]);
                  ("P95 (us)", "#38bdf8", [ 420.; 445.; 410.; 395.; 380.; 390.; 375. ]);
                  ("P99 (us)", "#f43f5e", [ 890.; 940.; 820.; 790.; 760.; 740.; 710. ]);
                ];
            ];
          spotlight_card
            ~title:"OHLC Candlestick & Shard Density Heatmap"
            ~subtitle:"Pure-SVG financial candlesticks + intensity matrix"
            [
              candlestick_chart ~width:500 ~height:110
                [
                  (100.0, 108.0, 96.0, 105.0);
                  (105.0, 112.0, 102.0, 109.0);
                  (109.0, 110.0, 99.0, 101.0);
                  (101.0, 115.0, 100.0, 113.0);
                  (113.0, 120.0, 110.0, 118.0);
                  (118.0, 119.0, 108.0, 111.0);
                  (111.0, 125.0, 110.0, 123.0);
                ];
              div ~style:Style.[ mt 10 ] [
                heatmap ~cols:8
                  [
                    12.; 28.; 45.; 82.; 95.; 88.; 64.; 30.;
                    18.; 34.; 59.; 91.; 99.; 92.; 70.; 42.;
                  ];
              ];
            ];
        ];
      div
        ~style:Style.[ grid_cols 2; gap 20 ]
        [
          spotlight_card
            ~title:"Subsystem Benchmark Radar"
            ~subtitle:"Multi-axis SVG spider chart"
            [
              div ~style:Style.[ flex_row; justify_around; items_center ] [
                radar_chart ~size:210
                  [
                    ("SSR Speed", 98.0);
                    ("Memory", 96.0);
                    ("Type Safety", 100.0);
                    ("WCAG Math", 94.0);
                    ("Zero-JS", 99.0);
                  ];
                timeline
                  [
                    ("00:01.2", "v3.0 Custom Renderers", "Compiled DOM tree to Canvas2D, Markdown & JSON");
                    ("00:00.8", "v2.2 Middleware & ISR", "Added Token-Bucket Rate Cop & TTL cache");
                    ("00:00.3", "v2.1 WCAG Contrast Engine", "Verified 100% AA relative luminance ratios");
                  ];
              ];
            ];
          spotlight_card
            ~title:"Regional Shard Throughput (Bar SVG)"
            ~subtitle:"Vector bar chart across active availability zones"
            [
              bar_chart ~width:500 ~height:150
                [ ("US-E", 94.0); ("EU-C", 88.0); ("APAC", 76.0); ("US-W", 99.0); ("SA-E", 64.0) ];
            ];
        ];
    ]

(* PAGE 3: Interactive Kanban Board, Time-Travel Undo/Redo & Web Components *)
let components_page _ =
  div
    ~style:Style.[ flex_col; gap 24; p 28; max_w 1200; mx_auto; animate_fade_in ]
    [
      h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Interactive Kanban, Time-Travel Lab & Web Components" ];
      spotlight_card
        ~title:"Reactive Kanban Board (Click 'Advance ->' to move tasks across lanes)"
        ~subtitle:"Backed by a single OCaml Signal list (`kanban_tasks`)"
        [
          kanban_board ~lanes:[ "Backlog"; "In Progress"; "Done" ] kanban_tasks;
        ];
      div
        ~style:Style.[ grid_cols 2; gap 20 ]
        [
          spotlight_card
            ~title:"Time-Travel Signal History (Undo / Redo)"
            ~subtitle:"Every state transition is recorded on an immutable history stack"
            [
              rx (fun () ->
                let cur_val = Signal.history_get counter_hist in
                let has_past = counter_hist.Signal.past <> [] in
                let has_future = counter_hist.Signal.future <> [] in
                div
                  ~style:Style.[ flex_col; gap 14 ]
                  [
                    div ~style:Style.[ flex_row; justify_between; items_center ] [
                      h2 ~style:Style.[ text_3xl; bold; color Palette.sky_400; m 0 ]
                        [ textf "Counter: %d" cur_val ];
                      div ~style:Style.[ flex_row; gap 8 ] [
                        badge ~variant:(if has_past then Primary else Neutral)
                          (if has_past then "Undo Ready" else "At Root");
                        badge ~variant:(if has_future then Success else Neutral)
                          (if has_future then "Redo Ready" else "Head");
                      ];
                    ];
                    progress_bar ~label:"Signal Magnitude"
                      ~pct:(float_of_int (min 100 (max 0 (cur_val * 2))))
                      ();
                    div ~style:Style.[ flex_row; gap 10 ] [
                      btn ~variant:Success
                        ~on_click:(fun () -> Signal.history_set counter_hist (cur_val + 5))
                        "+5 Step";
                      btn ~variant:Primary
                        ~on_click:(fun () -> Signal.history_set counter_hist (cur_val * 2))
                        "x2 Multiply";
                      btn ~variant:Neutral ~on_click:(fun () -> ignore (undo counter_hist)) "Undo";
                      btn ~variant:Neutral ~on_click:(fun () -> ignore (redo counter_hist)) "Redo";
                    ];
                  ]);
            ];
          spotlight_card
            ~title:"Mouse-Tracking Surface & Declarative Shadow DOM"
            ~subtitle:"Streams cursor coordinates + emits <q-telemetry-card> Web Component"
            [
              div
                ~attrs:[ Dom.on_mousemove (fun (x, y) -> set_cursor (x, y)) ]
                ~style:
                  Style.[
                    p 18;
                    rounded_xl;
                    bg Palette.slate_950;
                    border ~width:1 ~color:Palette.sky_500 ();
                    flex_row;
                    justify_between;
                    items_center;
                    mb 14;
                  ]
                [
                  span ~style:Style.[ text_sm; color Palette.slate_300 ]
                    [ text "Interactive Telemetry Surface (move cursor here)" ];
                  rx (fun () ->
                    let x, y = !~cursor_coords in
                    badge ~variant:Primary (Printf.sprintf "X: %d px | Y: %d px" x y));
                ];
              custom_element "q-telemetry-card"
                ~props:[ ("data-version", "3.0"); ("role", "region") ]
                ~shadow:
                  (shadow_root ~scoped_css:":host { display: block; }"
                     [ span ~style:Style.[ text_xs; color Palette.emerald_400 ] [ text "Declarative Shadow DOM Slot Active" ] ])
                [
                  accordion
                    [
                      ( "q1",
                        "How does the Custom Renderer Functor work?",
                        text "Pass any module implementing `Custom_renderer.BACKEND` to `Custom_renderer.Make` to compile `Dom.node` trees into Canvas2D, Markdown, JSON, or native GPU calls." );
                      ( "q2",
                        "How does WCAG 2.1 Color Contrast checking work?",
                        text "`Contrast.contrast_ratio` linearizes sRGB channels (`C <= 0.03928 ? C/12.92 : ((C+0.055)/1.055)^2.4`) and audits every styled DOM node during `quartz check`." );
                    ];
                ];
            ];
        ];
    ]

(* PAGE 4: Middleware, ISR, SSE & Custom Renderers *)
let network_page _ =
  div
    ~style:Style.[ flex_col; gap 24; p 28; max_w 1200; mx_auto; animate_fade_in ]
    [
      h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Middleware Pipeline, ISR, SSE & Custom Renderers" ];
      div
        ~style:Style.[ grid_cols 2; gap 20 ]
        [
          spotlight_card
            ~title:"Express / NestJS-Style Functional Middleware"
            ~subtitle:"Token-bucket Rate Cop + CORS + Security Headers + Microsecond Timer"
            [
              code_block ~lang:"ocaml"
                "use_middleware (Middleware.cors ());\nuse_middleware (Middleware.rate_cop ~max_burst:120. ~refill_per_sec:30. ());\nuse_middleware (Middleware.bearer_guard ~token:\"secret\");\nuse_middleware (Middleware.req_timer ());";
            ];
          spotlight_card
            ~title:"Multi-Target Custom Renderers"
            ~subtitle:"Render the exact same UI tree to HTML, TUI, Canvas2D, Markdown & JSON"
            [
              code_block ~lang:"bash"
                "dune exec quartz -- check     # 5-stage Doctor + WCAG Contrast Audit\ndune exec quartz -- build     # Static HTML/CSS + sitemap.xml + manifest\ndune exec quartz -- tui       # ANSI Flexbox Terminal UI\ndune exec quartz -- markdown  # GitHub-Flavored Markdown Exporter\ndune exec quartz -- canvas    # HTML5 Canvas2D GPU Draw Call Buffer";
              div ~style:Style.[ flex_row; gap 10; mt 12 ] [
                link ~to_:"/service/edge-auth-proxy"
                  ~style:Style.[ px 14; py 8; rounded_lg; bg Palette.sky_600; color Palette.white; text_sm; bold ]
                  [ text "Inspect edge-auth-proxy ->" ];
                link ~to_:"/analytics?region=eu-central"
                  ~style:Style.[ px 14; py 8; rounded_lg; bg Palette.slate_800; color Palette.slate_100; text_sm ]
                  [ text "Deep-Link EU Telemetry ->" ];
              ];
            ];
        ];
    ]

(* PAGE 5: Parameterized Dynamic Route (/service/:id) *)
let service_detail_page params =
  let srv_id = Option.value ~default:"unknown" (Router.param "id" params) in
  div
    ~style:Style.[ flex_col; gap 20; p 28; max_w 960; mx_auto; animate_fade_in ]
    [
      div ~style:Style.[ flex_row; justify_between; items_center ] [
        div ~style:Style.[ flex_col; gap 6 ] [
          badge ~variant:Primary "DYNAMIC ROUTE (:id)";
          h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text ("Service Inspector: " ^ srv_id) ];
        ];
        link ~to_:"/"
          ~style:Style.[ px 14; py 8; rounded_lg; bg Palette.slate_800; color Palette.sky_400; text_sm; bold ]
          [ text "<- Back to Control Plane" ];
      ];
      spotlight_card
        ~title:("Live Telemetry for " ^ srv_id)
        ~subtitle:"Extracted via `Router.param \"id\" params`"
        [
          sparkline ~width:680 ~height:120 ~stroke:"#10b981"
            [ 42.0; 48.0; 45.0; 54.0; 62.0; 59.0; 68.0; 74.0; 71.0; 79.0 ];
        ];
    ]

let app_routes =
  with_layout app_shell
    [
      route ~title:"Overview" "/" overview_page;
      route ~title:"Analytics" "/analytics" analytics_page;
      route ~title:"Kanban & Lab" "/components" components_page;
      route ~title:"Middleware & Renderers" "/network" network_page;
      route ~title:"Services" "/service" overview_page;
      route ~title:"Service Detail" "/service/:id" service_detail_page;
    ]

let studio_app () = Router.router app_routes

let sample_paths = [ "/"; "/analytics"; "/components"; "/network"; "/service/edge-auth-proxy" ]

let static_export_specs =
  [
    ("index.html", "/", studio_app);
    ("analytics.html", "/analytics", studio_app);
    ("components.html", "/components", studio_app);
    ("network.html", "/network", studio_app);
    ("service-edge.html", "/service/edge-auth-proxy", studio_app);
  ]

let () =
  setup_backend ();
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: ("dev" | "serve") :: port_s :: _ ->
    let port = Option.value ~default:8080 (int_of_string_opt port_s) in
    run_browser ~port ~title:"Quartz v3.0 Studio" studio_app
  | _ :: ("dev" | "serve") :: _ ->
    run_browser ~port:8080 ~title:"Quartz v3.0 Studio" studio_app
  | _ :: "check" :: _ ->
    run_vite_check ~port:8080 ~routes:app_routes ~sample_paths studio_app
  | _ :: "build" :: _ ->
    run_vite_build ~out_dir:"dist_site" ~title:"Quartz v3.0 Studio" static_export_specs
  | _ :: "tui" :: _ ->
    Router.navigate "/";
    run_terminal ~width:80 ~height:20 studio_app
  | _ :: "markdown" :: _ ->
    Router.navigate "/";
    print_endline (render_markdown (studio_app ()))
  | _ :: "canvas" :: _ ->
    Router.navigate "/";
    print_endline (render_canvas2d ~width:800 ~height:400 (studio_app ()))
  | _ :: "json" :: _ ->
    Router.navigate "/";
    print_endline (render_json_scene (studio_app ()))
  | _ ->
    (* Default showcase pipeline: run 5-stage Doctor, build static bundle + SEO assets, and render TUI *)
    run_vite_check ~port:8080 ~routes:app_routes ~sample_paths studio_app;
    run_vite_build ~out_dir:"dist_site" ~title:"Quartz v3.0 Studio" static_export_specs;
    Printf.printf "  >>> [TUI Renderer] Rendering `/` via Flexbox Terminal Canvas <<<\n";
    Router.navigate "/";
    run_terminal ~width:80 ~height:20 studio_app
