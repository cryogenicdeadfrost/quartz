(* lib/runtime/unix/tooling/scaffolder.ml — Project Template Scaffolder (`quartz create`)
   Provides 6 Vite/Node-style starter builders with complete, runnable OCaml + Dune files. *)

type template_info = {
  id : string;
  title : string;
  description : string;
  stack : string;
}

let available_templates : template_info list = [
  {
    id = "minimal";
    title = "Minimal Reactive SPA";
    description = "Fine-grained Signals, validated Form, and SSR + client diff patching.";
    stack = "quartz.core + quartz.unix";
  };
  {
    id = "fullstack-saas";
    title = "Full-Stack SQLite SaaS Starter";
    description = "Parameterized SQL (node:sqlite / WAL), signed sessions, CRUD dashboard, and RPC.";
    stack = "quartz + Quartz.Sql + Quartz.Db_driver";
  };
  {
    id = "static-blog";
    title = "GitHub Pages Docs & Blog";
    description = "Zero-JS-by-default static site with interactive islands, sitemap, llms.txt, and GH Pages CI.";
    stack = "quartz + Static Export + GitHub Actions";
  };
  {
    id = "realtime-collab";
    title = "Realtime WebSocket & SSE Room";
    description = "RFC 6455 WebSocket rooms, PubSub topics, and live Server-Sent Events telemetry.";
    stack = "quartz + Websocket + Pubsub";
  };
  {
    id = "webgl-3d";
    title = "Creative 3D WebGL2/WebGPU Studio";
    description = "Declarative 3D meshes, custom GLSL/WGSL shaders, RK4 spring physics, and magnetic cursor.";
    stack = "quartz + Scene3d + Style.solve_spring_frames";
  };
  {
    id = "js-hybrid";
    title = "OCaml + NPM Universal Plug App";
    description = "Zero-porting ESM client widgets (Npm_plug) and persistent Node worker IPC (Npm_server_plug).";
    stack = "quartz + Npm_plug + Npm_server_plug";
  };
]

let rec mkdir_p path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_text path content =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let common_dune_project project_name =
  Printf.sprintf
{|(lang dune 3.0)
(name %s)
(generate_opam_files true)

(package
 (name %s)
 (synopsis "Built with Quartz Full-Stack OCaml Framework")
 (depends
  (ocaml (>= 4.13.0))
  dune
  quartz))
|} project_name project_name

let common_bin_dune project_name =
  Printf.sprintf
{|(executable
 (name main)
 (public_name %s)
 (libraries quartz unix threads))
|} project_name

let common_gitignore =
{|_build/
*.install
.merlin
*.wal
*.db
node_modules/
dist_site/
|}

let template_minimal_main project_name =
  Printf.sprintf
{|(* %s — Minimal Reactive SPA Starter *)
open Quartz

let count = use_signal 0
let email_field = Form.use_field ~label:"Email" ~validators:[ Form.required; Form.email ] ""

let view () =
  div ~style:Style.[ p 32; max_w 720; mx_auto; flex_col; gap 16 ] [
    h1 ~style:Style.[ text_3xl; bold; color Palette.indigo_400 ] [
      text "%s"
    ];
    p ~style:Style.[ color Palette.slate_400 ] [
      text "Reactive OCaml Signal counter + validated form running over Quartz."
    ];
    div ~style:Style.[ p 24; rounded 12; bg Palette.slate_900; flex_row; items_center; gap 16 ] [
      btn ~on_click:(fun () -> Signal.update count (fun n -> n + 1)) "Increment";
      rx_text (fun () -> Printf.sprintf "Count = %%d" !~count);
    ];
    Form.input_view email_field;
  ]

let routes = [
  route ~title:"Home" "/" (fun _ -> view ());
]

let () =
  run_browser ~port:8080 ~title:"%s" ~routes view
|} project_name project_name project_name

let template_saas_main project_name =
  Printf.sprintf
{|(* %s — Full-Stack SQLite SaaS Starter *)
open Quartz

let db = Db_driver.connect (Db_driver.Node_sqlite "saas_app.db")

let () =
  ignore (Db_driver.migrate db [
    (1, "create_tasks",
     "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY, title TEXT NOT NULL, status TEXT NOT NULL);");
  ]);
  match Db_driver.query db (Sql.q "SELECT * FROM tasks" []) with
  | Ok [] ->
    ignore (Db_driver.exec db
      (Sql.q "INSERT INTO tasks (title, status) VALUES (?, ?)"
        [ Sql.Text "Provision production database"; Sql.Text "done" ]));
    ignore (Db_driver.exec db
      (Sql.q "INSERT INTO tasks (title, status) VALUES (?, ?)"
        [ Sql.Text "Configure signed cookie auth"; Sql.Text "active" ]))
  | _ -> ()

let filter_status = use_signal "all"

let render_tasks () =
  let query =
    if !~filter_status = "all" then
      Sql.q "SELECT * FROM tasks ORDER BY id DESC" []
    else
      Sql.q "SELECT * FROM tasks WHERE status = ? ORDER BY id DESC"
        [ Sql.Text !~filter_status ]
  in
  let rows = match Db_driver.query db query with Ok r -> r | Error _ -> [] in
  ul ~style:Style.[ flex_col; gap 8 ]
    (List.map (fun r ->
      let id = Option.value (Sql.get_int "id" r) ~default:0 in
      let title = Option.value (Sql.get_string "title" r) ~default:"Untitled" in
      let status = Option.value (Sql.get_string "status" r) ~default:"active" in
      li ~key:(string_of_int id)
        ~style:Style.[ p 16; rounded 8; bg Palette.slate_900; flex_row; justify_between; items_center ]
        [
          span ~style:Style.[ font_bold ] [ text title ];
          badge status;
        ]
    ) rows)

let view () =
  div ~style:Style.[ p 32; max_w 860; mx_auto; flex_col; gap 20 ] [
    header ~style:Style.[ flex_row; justify_between; items_center ] [
      h1 ~style:Style.[ text_2xl; bold ] [ text "%s — SaaS Dashboard" ];
      div ~style:Style.[ flex_row; gap 8 ] [
        btn ~on_click:(fun () -> Signal.set filter_status "all") "All";
        btn ~on_click:(fun () ->
          ignore (Db_driver.exec db
            (Sql.q "INSERT INTO tasks (title, status) VALUES (?, ?)"
              [ Sql.Text "New task created from UI"; Sql.Text "active" ]));
          Signal.set filter_status "all") "+ Add Task";
      ];
    ];
    rx render_tasks;
  ]

let routes = [
  route ~title:"Dashboard" "/" (fun _ -> view ());
]

let () =
  run_browser ~port:8080 ~title:"%s SaaS" ~routes view
|} project_name project_name project_name

let template_blog_main project_name =
  Printf.sprintf
{|(* %s — GitHub Pages Static Blog & Docs Starter *)
open Quartz

type post = {
  slug : string;
  title : string;
  date : string;
  summary : string;
  body : string;
}

let posts = [
  {
    slug = "type-safe-ssr";
    title = "Why We Built Our Docs in Pure OCaml";
    date = "2026-11-15";
    summary = "Shipping 0 KB JS by default with selective WebGL2 and interactive islands.";
    body = "Static pages render ahead-of-time via Quartz.Vite, while interactive islands hydrate cleanly on GitHub Pages.";
  };
  {
    slug = "sql-without-orm-bloat";
    title = "Parameterized SQL in 200 Lines of OCaml";
    date = "2026-11-10";
    summary = "Dialect-aware queries across SQLite, PostgreSQL, and Turso Hrana HTTP.";
    body = "Using Quartz.Sql, queries compile to $1..$n for Postgres or ? for SQLite with full injection safety.";
  };
]

let selected_slug = use_signal "type-safe-ssr"

let view () =
  let current =
    match List.find_opt (fun p -> p.slug = !~selected_slug) posts with
    | Some p -> p
    | None -> List.hd posts
  in
  div ~style:Style.[ p 32; max_w 960; mx_auto; flex_col; gap 24 ] [
    h1 ~style:Style.[ text_3xl; bold; color Palette.indigo_400 ] [ text "%s Articles" ];
    div ~style:Style.[ flex_row; gap 12 ] (
      List.map (fun p ->
        btn ~on_click:(fun () -> Signal.set selected_slug p.slug) p.title
      ) posts
    );
    article ~style:Style.[ p 24; rounded 12; bg Palette.slate_900; flex_col; gap 12 ] [
      span ~style:Style.[ text_xs; font_mono; color Palette.indigo_400 ] [ text current.date ];
      h2 ~style:Style.[ text_2xl; bold ] [ text current.title ];
      p ~style:Style.[ color Palette.slate_300 ] [ text current.body ];
    ];
  ]

let routes = [
  route ~title:"Blog" "/" (fun _ -> view ());
]

let () =
  match Array.to_list Sys.argv with
  | _ :: "export" :: out_dir :: _ ->
    let pages = [ ("index.html", "/", view) ] in
    Vite.run_vite_build ~out_dir ~base_url:"https://example.github.io" ~title:"%s Blog" pages
  | _ ->
    run_browser ~port:8080 ~title:"%s Blog" ~routes view
|} project_name project_name project_name project_name

let template_realtime_main project_name =
  Printf.sprintf
{|(* %s — Realtime WebSocket & SSE Room Starter *)
open Quartz

let messages : string list Signal.t = use_signal [ "System: Room initialized." ]

let view () =
  div ~style:Style.[ p 32; max_w 780; mx_auto; flex_col; gap 16 ] [
    h1 ~style:Style.[ text_2xl; bold; color Palette.emerald_400 ] [
      text "%s — Realtime PubSub & SSE Board"
    ];
    btn ~on_click:(fun () ->
      let msg = Printf.sprintf "Event #%%d broadcasted" (List.length !~messages) in
      Pubsub.publish ~topic:"telemetry" msg;
      Signal.update messages (fun ms -> msg :: ms))
      "Broadcast Event";
    ul ~style:Style.[ flex_col; gap 8 ]
      (List.mapi (fun i m ->
        li ~key:(string_of_int i)
          ~style:Style.[ p 12; rounded 8; bg Palette.slate_900; font_mono; text_sm ]
          [ text m ]
      ) !~messages);
  ]

let routes = [ route ~title:"Realtime" "/" (fun _ -> view ()) ]

let () =
  run_browser ~port:8080 ~title:"%s Realtime" ~routes view
|} project_name project_name project_name

let template_webgl_main project_name =
  Printf.sprintf
{|(* %s — Creative 3D WebGL2/WebGPU Studio Starter *)
open Quartz

let mesh_choice = use_signal 0

let current_scene () =
  let geometry =
    match !~mesh_choice mod 3 with
    | 0 -> Scene3d.Torus { radius = 1.1; tube = 0.38 }
    | 1 -> Scene3d.Sphere { radius = 1.25; segments = 28 }
    | _ -> Scene3d.Box { w = 1.5; h = 1.5; d = 1.5 }
  in
  Scene3d.create
    ~renderer:`WebGL2
    [ Scene3d.mesh ~geometry () ]

let view () =
  div ~style:Style.[ p 32; max_w 860; mx_auto; flex_col; gap 20 ] [
    div ~style:Style.[ flex_row; justify_between; items_center ] [
      h1 ~style:Style.[ text_2xl; bold; color Palette.indigo_400 ] [ text "%s — 3D Shader Studio" ];
      btn ~on_click:(fun () -> Signal.update mesh_choice (fun n -> n + 1)) "Cycle 3D Geometry";
    ];
    canvas3d ~width:720 ~height:420 (current_scene ());
  ]

let routes = [ route ~title:"Studio" "/" (fun _ -> view ()) ]

let () =
  run_browser ~port:8080 ~title:"%s 3D Studio" ~routes view
|} project_name project_name project_name

let template_hybrid_main project_name =
  Printf.sprintf
{|(* %s — OCaml + NPM Universal Plug Hybrid Starter *)
open Quartz

let chart_plug =
  Npm_plug.component
    ~pkg:"chart.js"
    ~version:"4.4.1"
    ~export_name:"Chart"
    ~framework:`Vanilla
    ~props:[
      ("type", Json.String "bar");
      ("labels", Json.Array [ Json.String "Q1"; Json.String "Q2"; Json.String "Q3"; Json.String "Q4" ]);
    ]
    ()

let node_hash =
  match
    Npm_server_plug.eval_js
      ~expr_fn:"(args, require) => require('crypto').createHash('sha256').update(args).digest('hex')"
      ~args_json:(Json.to_string (Json.String "%s"))
  with
  | Ok raw ->
    (match Json.parse raw with
     | Ok (Json.String h) -> String.sub h 0 16
     | _ -> "offline-fallback")
  | _ -> "offline-fallback"

let view () =
  div ~style:Style.[ p 32; max_w 780; mx_auto; flex_col; gap 16 ] [
    h1 ~style:Style.[ text_2xl; bold; color Palette.amber_400 ] [
      text "%s — OCaml + NPM Universal Plug"
    ];
    p ~style:Style.[ text_sm; font_mono; color Palette.slate_400 ] [
      text ("Node worker SHA-256 prefix: " ^ node_hash)
    ];
    div ~style:Style.[ p 24; rounded 12; bg Palette.slate_900 ] [
      chart_plug
    ];
  ]

let routes = [ route ~title:"Hybrid" "/" (fun _ -> view ()) ]

let () =
  run_browser ~port:8080 ~title:"%s Hybrid" ~routes view
|} project_name project_name project_name project_name

let scaffold_project ~(template_id : string) ~(target_dir : string) : (string list, string) result =
  let norm_id = String.lowercase_ascii (String.trim template_id) in
  match List.find_opt (fun t -> t.id = norm_id) available_templates with
  | None ->
    let valid = String.concat ", " (List.map (fun t -> t.id) available_templates) in
    Error (Printf.sprintf "Unknown template '%s'. Available templates: %s" template_id valid)
  | Some tmpl ->
    let raw_name = Filename.basename target_dir in
    let project_name =
      let buf = Buffer.create (String.length raw_name) in
      String.iter (fun c ->
        if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' then
          Buffer.add_char buf c
        else if c >= 'A' && c <= 'Z' then
          Buffer.add_char buf (Char.lowercase_ascii c)
        else
          Buffer.add_char buf '_'
      ) raw_name;
      let s = Buffer.contents buf in
      if s = "" || (s.[0] >= '0' && s.[0] <= '9') then "quartz_app_" ^ s else s
    in
    let main_ml =
      match tmpl.id with
      | "minimal" -> template_minimal_main project_name
      | "fullstack-saas" -> template_saas_main project_name
      | "static-blog" -> template_blog_main project_name
      | "realtime-collab" -> template_realtime_main project_name
      | "webgl-3d" -> template_webgl_main project_name
      | "js-hybrid" -> template_hybrid_main project_name
      | _ -> template_minimal_main project_name
    in
    let readme =
      Printf.sprintf
{|# %s

Generated from the Quartz `%s` template (%s).

## Quickstart

```bash
opam exec -- dune build
opam exec -- dune exec %s
```

Then open `http://127.0.0.1:8080`.
|} project_name tmpl.id tmpl.title project_name
    in
    let files = [
      (Filename.concat target_dir "dune-project", common_dune_project project_name);
      (Filename.concat target_dir ".gitignore", common_gitignore);
      (Filename.concat target_dir "README.md", readme);
      (Filename.concat (Filename.concat target_dir "bin") "dune", common_bin_dune project_name);
      (Filename.concat (Filename.concat target_dir "bin") "main.ml", main_ml);
    ] in
    let extra_files =
      if tmpl.id = "static-blog" then
        let gh_pages_yml = Deploy.github_pages_workflow_yaml ~out_dir:"dist_site" () in
        [ (Filename.concat target_dir ".github/workflows/gh-pages.yml", gh_pages_yml) ]
      else []
    in
    let all_files = files @ extra_files in
    List.iter (fun (path, content) -> write_text path content) all_files;
    Ok (List.map fst all_files)
