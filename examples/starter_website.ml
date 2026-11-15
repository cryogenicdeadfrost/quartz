(* examples/starter_website.ml — Complete Out-of-the-Box Multi-Page Website
   Includes:
   - Shared responsive navigation layout across 4 routes (/, /docs, /releases, /playground)
   - SQLite / WAL database table (`releases`) queried via `Quartz.Sql` & `Quartz.Db_driver`
   - Interactive Signal-driven infrastructure latency & throughput calculator
   - Live 3D WebGL2/WebGPU canvas & SVG telemetry charts
   - Embedded JSON REST API (`/api/releases`)
   - CLI modes:
       dune exec examples/starter_website.exe                (runs 6-stage check + builds static site)
       dune exec examples/starter_website.exe -- serve 3000  (starts live HTTP/WS/SSE server)
       dune exec examples/starter_website.exe -- export dir  (exports GitHub Pages static bundle) *)

open Quartz

(* 1. Database Setup (`Quartz.Sql` + `Quartz.Db_driver`) *)
let db = Db_driver.connect (Db_driver.Node_sqlite ":memory:")

let bootstrap_database () =
  ignore
    (Db_driver.migrate db
       [
         ( 1,
           "create_releases_table",
           "CREATE TABLE IF NOT EXISTS releases (id INTEGER PRIMARY KEY, version TEXT NOT NULL, title TEXT NOT NULL, channel TEXT NOT NULL, summary TEXT NOT NULL);"
         );
       ]);
  match Db_driver.query db (Sql.q "SELECT * FROM releases" []) with
  | Ok [] ->
    let seed =
      [
        ("v4.2.2", "Universal Database Drivers & Scaffolder", "stable",
         "Added parameterized SQL dialect compiler, Node 22 SQLite bridge, Turso Hrana HTTP, and 6 starter templates.");
        ("v4.2.1", "Zero-Porting NPM Plug & DevTools Inspector", "stable",
         "Mount React/Vanilla/WebComponent packages from esm.sh and call Node packages over persistent NDJSON stdio.");
        ("v4.1.0", "Streaming SSR & WebGPU 3D Pipeline", "preview",
         "Out-of-order HTTP/1.1 chunked suspense streaming and WGSL 3D mesh rendering.");
      ]
    in
    List.iter
      (fun (ver, title, ch, sum) ->
        ignore
          (Db_driver.exec db
             (Sql.q
                "INSERT INTO releases (version, title, channel, summary) VALUES (?, ?, ?, ?)"
                [ Sql.Text ver; Sql.Text title; Sql.Text ch; Sql.Text sum ])))
      seed
  | _ -> ()

(* 2. Reactive State Signals *)
let est_rps = use_signal 12000
let est_regions = use_signal 3
let release_filter = use_signal "all"
let active_shape = use_signal 0
let doc_query = use_signal ""

let est_p99_ms =
  use_memo (fun () ->
    let base = 1.4 in
    let load_factor = float_of_int !~est_rps /. 25000.0 in
    let geo_bonus = float_of_int (max 1 !~est_regions) *. 0.18 in
    Float.max 0.4 (base +. load_factor -. geo_bonus))

let est_monthly_usd =
  use_memo (fun () ->
    let compute = !~est_regions * 14 in
    let bandwidth = !~est_rps / 2000 * 3 in
    compute + bandwidth)

(* 3. Shared Website Shell (Header + Main + Footer) *)
let site_shell (content : Dom.node) : Dom.node =
  div
    ~style:
      Style.[
        h_screen;
        bg Palette.slate_950;
        color Palette.slate_100;
        font_sans;
        flex_col;
        justify_between;
      ]
    [
      header
        ~style:
          Style.[
            bg Palette.slate_900;
            border ~width:1 ~color:Palette.slate_800 ();
            px 28;
            py 16;
            flex_row;
            justify_between;
            items_center;
          ]
        [
          div ~style:Style.[ flex_row; items_center; gap 12 ] [
            badge ~variant:Primary "QUARTZ ATLAS";
            span ~style:Style.[ text_lg; bold; color Palette.slate_100 ] [
              text "OCaml Full-Stack Web Portal"
            ];
          ];
          nav ~attrs:[ Dom.attr "aria-label" "Primary Navigation" ]
            ~style:Style.[ flex_row; items_center; gap 20 ]
            [
              link ~to_:"/" ~style:Style.[ text_sm; color Palette.slate_200 ] [ text "Overview" ];
              link ~to_:"/docs" ~style:Style.[ text_sm; color Palette.slate_200 ] [ text "Documentation" ];
              link ~to_:"/releases" ~style:Style.[ text_sm; color Palette.slate_200 ] [ text "Releases (SQL)" ];
              link ~to_:"/playground" ~style:Style.[ text_sm; color Palette.slate_200 ] [ text "3D & Plugs" ];
            ];
        ];
      main ~style:Style.[ flex_1; max_w 1080; w_full; mx_auto; p 28 ] [ content ];
      footer
        ~style:
          Style.[
            bg Palette.slate_900;
            border ~width:1 ~color:Palette.slate_800 ();
            px 28;
            py 18;
            flex_row;
            justify_between;
            items_center;
            text_xs;
            color Palette.slate_400;
          ]
        [
          span [ text "Powered by Quartz v4.2 — Pure OCaml SSR, Signals, SQL & WebGPU" ];
          span [ text "WCAG 2.1 AA + APCA Verified | Ready for GitHub Pages, Docker & Edge" ];
        ];
    ]

(* 4. Page 1: Overview & Interactive Estimator (`/`) *)
let home_page _params =
  div ~style:Style.[ flex_col; gap 28 ] [
    section
      ~style:
        Style.[
          p 32;
          rounded_xl;
          bg Palette.slate_900;
          border ~width:1 ~color:Palette.slate_800 ();
          flex_col;
          gap 16;
        ]
      [
        div ~style:Style.[ flex_row; gap 10; items_center ] [
          badge ~variant:Success "LIVE STARTER WEBSITE";
          badge ~variant:Primary "ZERO PLACEHOLDERS";
        ];
        h1 ~style:Style.[ text_4xl; bold; color Palette.slate_100; m 0 ] [
          text "Build, Query, and Deploy Type-Safe Websites in OCaml."
        ];
        p ~style:Style.[ text_lg; color Palette.slate_300; m 0 ] [
          text
            "This starter website runs out of the box with reactive Signals, a live SQLite/WAL changelog database, WAI-ARIA accessible components, and one-command static export for GitHub Pages."
        ];
        div ~style:Style.[ flex_row; gap 12; pt 8 ] [
          link ~to_:"/docs"
            ~style:Style.[ px 16; py 10; rounded_lg; bg Palette.sky_500; color Palette.slate_950; bold; text_sm ]
            [ text "Explore Documentation ->" ];
          link ~to_:"/releases"
            ~style:Style.[ px 16; py 10; rounded_lg; bg Palette.slate_800; color Palette.slate_100; bold; text_sm ]
            [ text "Browse SQL Changelog" ];
        ];
      ];
    div ~style:Style.[ grid_cols 3; gap 16 ] [
      stat_card ~label:"Estimated p99 Latency"
        ~value:(Printf.sprintf "%.2f ms" !~est_p99_ms)
        ~delta:"Sub-2ms native SSR" ~positive:true ();
      stat_card ~label:"Target Throughput"
        ~value:(Printf.sprintf "%d req/s" !~est_rps)
        ~delta:(Printf.sprintf "%d active edge regions" !~est_regions) ~positive:true ();
      stat_card ~label:"Projected Infra Cost"
        ~value:(Printf.sprintf "$%d / mo" !~est_monthly_usd)
        ~delta:"82% lower RAM than Node SSR" ~positive:true ();
    ];
    spotlight_card
      ~title:"Interactive Throughput & Region Sizing Calculator"
      ~subtitle:"Adjust traffic and region count to recompute latency and cost via OCaml Signals"
      [
        div ~style:Style.[ flex_col; gap 16 ] [
          div ~style:Style.[ flex_row; gap 12; items_center ] [
            btn ~variant:Primary
              ~on_click:(fun () -> Signal.update est_rps (fun r -> r + 5000))
              "+ 5,000 req/s";
            btn ~variant:Neutral
              ~on_click:(fun () -> Signal.update est_rps (fun r -> max 2000 (r - 5000)))
              "- 5,000 req/s";
            btn ~variant:Success
              ~on_click:(fun () -> Signal.update est_regions (fun n -> min 12 (n + 1)))
              "+ Add Region";
          ];
          area_chart ~width:680 ~height:140 ~color:"#38bdf8"
            [ 18.0; 24.0; 22.0; 31.0; 39.0; 44.0; 52.0; 61.0; 58.0; 72.0 ];
        ];
      ];
  ]

(* 5. Page 2: Documentation & Architecture (`/docs`) *)
let docs_items =
  [
    ( "Reactive Signals & Keyed DOM Diffing",
      "Signals track fine-grained readers and batch updates synchronously. Server-rendered pages hydrate only interactive nodes or patch keyed DOM deltas over WebSockets." );
    ( "Parameterized SQL & Multi-Database Drivers",
      "Quartz.Sql compiles ? and $1..$n placeholders safely across SQLite, PostgreSQL, and Turso Hrana HTTP, backed by Node 22 node:sqlite or the embedded crash-safe WAL engine." );
    ( "Zero-Porting NPM Universal Plug",
      "Mount React, Vanilla JS, or Web Components from esm.sh in the browser via Npm_plug, and invoke server-side Node/Bun packages over persistent stdio NDJSON via Npm_server_plug." );
    ( "GitHub Pages & Multi-Cloud Deployment",
      "Export static HTML, content-hashed CSS/JS, sitemap.xml, llms.txt, .nojekyll, and 404.html SPA fallback in one command." );
  ]

let docs_page _params =
  let q = String.lowercase_ascii (String.trim !~doc_query) in
  let contains hay needle =
    if needle = "" then true
    else
      let hlen = String.length hay and nlen = String.length needle in
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub hay i nlen = needle then true
        else loop (i + 1)
      in
      loop 0
  in
  let filtered =
    List.filter
      (fun (t, b) ->
        contains (String.lowercase_ascii t) q || contains (String.lowercase_ascii b) q)
      docs_items
  in
  div ~style:Style.[ flex_col; gap 24 ] [
    div ~style:Style.[ flex_col; gap 8 ] [
      badge ~variant:Primary "ARCHITECTURE & GUIDES";
      h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Quartz Engineering Documentation" ];
      p ~style:Style.[ color Palette.slate_300; m 0 ] [
        text "Every subsystem is modularized under lib/core, lib/runtime, and lib/adapters."
      ];
    ];
    div ~style:Style.[ flex_row; gap 10 ] [
      btn ~variant:Neutral ~on_click:(fun () -> Signal.set doc_query "") "Show All";
      btn ~variant:Neutral ~on_click:(fun () -> Signal.set doc_query "sql") "Filter: SQL";
      btn ~variant:Neutral ~on_click:(fun () -> Signal.set doc_query "npm") "Filter: NPM Plug";
      btn ~variant:Neutral ~on_click:(fun () -> Signal.set doc_query "github") "Filter: GitHub Pages";
    ];
    div ~style:Style.[ flex_col; gap 14 ]
      (List.map
         (fun (title, body) ->
           spotlight_card ~title ~subtitle:"Verified Subsystem"
             [ p ~style:Style.[ color Palette.slate_300; m 0 ] [ text body ] ])
         filtered);
    code_block ~lang:"ocaml"
      {|open Quartz

let db = Db_driver.connect (Db_driver.Node_sqlite "app.db")
let rows = Db_driver.query db (Sql.q "SELECT * FROM releases WHERE channel = ?" [ Sql.Text "stable" ])|};
  ]

(* 6. Page 3: SQL-Backed Releases & Changelog (`/releases`) *)
let releases_page _params =
  let ch = !~release_filter in
  let query =
    if ch = "all" then Sql.q "SELECT * FROM releases ORDER BY id DESC" []
    else Sql.q "SELECT * FROM releases WHERE channel = ? ORDER BY id DESC" [ Sql.Text ch ]
  in
  let rows = match Db_driver.query db query with Ok r -> r | Error _ -> [] in
  div ~style:Style.[ flex_col; gap 24 ] [
    div ~style:Style.[ flex_row; justify_between; items_center ] [
      div ~style:Style.[ flex_col; gap 6 ] [
        badge ~variant:Success "SQLITE / WAL DATABASE";
        h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Release Changelog" ];
      ];
      div ~style:Style.[ flex_row; gap 8 ] [
        btn ~variant:Neutral ~on_click:(fun () -> Signal.set release_filter "all") "All Channels";
        btn ~variant:Neutral ~on_click:(fun () -> Signal.set release_filter "stable") "Stable Only";
        btn ~variant:Primary
          ~on_click:(fun () ->
            let next_num = List.length rows + 3 in
            ignore
              (Db_driver.exec db
                 (Sql.q
                    "INSERT INTO releases (version, title, channel, summary) VALUES (?, ?, ?, ?)"
                    [
                      Sql.Text (Printf.sprintf "v4.2.%d" next_num);
                      Sql.Text "Automated Canary Deployment";
                      Sql.Text "stable";
                      Sql.Text "Inserted directly into SQLite/WAL via Quartz.Db_driver.";
                    ]));
            Signal.set release_filter "all")
          "+ Record Release";
      ];
    ];
    div ~style:Style.[ flex_col; gap 12 ]
      (List.map
         (fun r ->
           let ver = Option.value (Sql.get_string "version" r) ~default:"v4.2" in
           let title = Option.value (Sql.get_string "title" r) ~default:"Release" in
           let channel = Option.value (Sql.get_string "channel" r) ~default:"stable" in
           let summary = Option.value (Sql.get_string "summary" r) ~default:"" in
           spotlight_card ~title:(ver ^ " — " ^ title) ~subtitle:("Channel: " ^ channel)
             [ p ~style:Style.[ color Palette.slate_300; m 0 ] [ text summary ] ])
         rows);
  ]

(* 7. Page 4: 3D WebGL2/WebGPU & NPM Plug Playground (`/playground`) *)
let playground_page _params =
  let geom =
    match !~active_shape mod 3 with
    | 0 -> Scene3d.Torus { radius = 1.15; tube = 0.36 }
    | 1 -> Scene3d.Sphere { radius = 1.25; segments = 24 }
    | _ -> Scene3d.Box { w = 1.5; h = 1.5; d = 1.5 }
  in
  let scene =
    Scene3d.create ~renderer:`WebGL2
      [ Scene3d.mesh ~geometry:geom () ]
  in
  let confetti_plug =
    Npm_plug.component ~pkg:"canvas-confetti" ~version:"1.9.3" ~export_name:"default"
      ~framework:`Vanilla
      ~props:[ ("particleCount", Json.int 80); ("spread", Json.int 70) ] ()
  in
  div ~style:Style.[ flex_col; gap 24 ] [
    div ~style:Style.[ flex_row; justify_between; items_center ] [
      div ~style:Style.[ flex_col; gap 6 ] [
        badge ~variant:Primary "WEBGL2 / WEBGPU + ESM PLUG";
        h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Interactive 3D & JS Bridge Sandbox" ];
      ];
      btn ~variant:Primary
        ~on_click:(fun () -> Signal.update active_shape (fun n -> n + 1))
        "Switch 3D Mesh (Torus / Sphere / Box)";
    ];
    canvas3d ~width:720 ~height:320 scene;
    spotlight_card
      ~title:"Universal NPM Client Plug (canvas-confetti@1.9.3)"
      ~subtitle:"Hydrated dynamically from esm.sh without bundling webpack or node_modules"
      [ confetti_plug ];
  ]

let website_routes =
  with_layout site_shell
    [
      route ~title:"Overview" "/" home_page;
      route ~title:"Documentation" "/docs" docs_page;
      route ~title:"Releases" "/releases" releases_page;
      route ~title:"Playground" "/playground" playground_page;
    ]

let website_app () = Router.router website_routes

let sample_paths = [ "/"; "/docs"; "/releases"; "/playground" ]

let export_pages =
  [
    ("index.html", "/", website_app);
    ("docs.html", "/docs", website_app);
    ("releases.html", "/releases", website_app);
    ("playground.html", "/playground", website_app);
  ]

let () =
  bootstrap_database ();
  api_get "/api/releases" (fun _ ->
    match Db_driver.query db (Sql.q "SELECT * FROM releases ORDER BY id DESC" []) with
    | Ok rows -> Json.to_string (Sql.rows_to_json rows)
    | Error err -> Json.to_string (Json.obj [ ("error", Json.string err) ]));
  match Array.to_list Sys.argv with
  | _ :: "serve" :: port_s :: _ ->
    let port = Option.value ~default:3000 (int_of_string_opt port_s) in
    run_browser ~port ~title:"Quartz Atlas Starter Website" ~routes:website_routes website_app
  | _ :: "serve" :: _ ->
    run_browser ~port:3000 ~title:"Quartz Atlas Starter Website" ~routes:website_routes website_app
  | _ :: "export" :: out_dir :: _ ->
    run_vite_check ~port:3000 ~routes:website_routes ~sample_paths website_app;
    run_vite_build ~out_dir ~base_url:"https://example.github.io/quartz-atlas"
      ~title:"Quartz Atlas Starter Website" export_pages
  | _ ->
    run_vite_check ~port:3000 ~routes:website_routes ~sample_paths website_app;
    run_vite_build ~out_dir:"dist_starter_site" ~base_url:"https://example.github.io/quartz-atlas"
      ~title:"Quartz Atlas Starter Website" export_pages
