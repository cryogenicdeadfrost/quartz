(* One-Command Deployment Generators & Starter Templates (`Quartz.Deploy`).
   Generates production configs for:
   - GitHub Pages (`.github/workflows/pages.yml`)
   - Vercel (`.vercel/output/config.json`, static, Edge JS OR Custom Native bootstrap)
   - Cloudflare Workers (`wrangler.toml` + WinterCG `worker.js`)
   - Fly.io / Cloud Run / Docker (multi-stage static musl `Dockerfile` + `fly.toml`)
   - MirageOS 4 Unikernels (`config.ml` + `unikernel.ml`)
   - `quartz new <template>` scaffolds + `llms.txt` AI docs *)

open Quartz_core

let rec ensure_dir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then (
    ensure_dir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  )

let write_file path content =
  ensure_dir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let emit_github_pages ?(out_dir = ".") ?(dist_dir = "dist_site") () =
  let yml =
    Printf.sprintf
      {|name: Deploy Quartz Static Site to GitHub Pages

on:
  push:
    branches: [ main ]
  workflow_dispatch:

permissions:
  contents: read
  pages: write
  id-token: write

jobs:
  build_and_deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: 5.2.x
      - run: opam install . --deps-only -y
      - run: dune build
      - run: dune runtest
      - run: dune exec quartz -- check
      - run: dune exec quartz -- build
      - uses: actions/upload-pages-artifact@v3
        with:
          path: %s
      - id: deployment
        uses: actions/deploy-pages@v4
|}
      dist_dir
  in
  let target = Filename.concat out_dir ".github/workflows/pages.yml" in
  write_file target yml;
  target

let emit_vercel_output ?(out_dir = ".vercel/output") ?(mode = `Edge) ?(app_module = "./quartz_app.bc.js") () =
  let static_dir = Filename.concat out_dir "static" in
  let func_dir = Filename.concat out_dir "functions/index.func" in
  ensure_dir_p static_dir;
  ensure_dir_p func_dir;
  let config_json =
    Json.to_pretty_string
      (Json.obj
         [
           ("version", Json.int 3);
           ( "routes",
             Json.array
               [
                 Json.obj
                   [
                     ("src", Json.string "^/assets/(.*)$");
                     ("headers", Json.obj [ ("cache-control", Json.string "public, max-age=31536000, immutable") ]);
                   ];
                 Json.obj [ ("handle", Json.string "filesystem") ];
                 Json.obj [ ("src", Json.string "/(.*)"); ("dest", Json.string "/index") ];
               ] );
         ])
  in
  write_file (Filename.concat out_dir "config.json") config_json;
  (match mode with
   | `Edge ->
     let vc_cfg =
       Json.to_pretty_string
         (Json.obj
            [
              ("runtime", Json.string "edge");
              ("entrypoint", Json.string "index.js");
            ])
     in
     let js_code =
       Printf.sprintf
         {|import { handleFetchJson } from %S;

export default async function handler(request) {
  const hdrs = {};
  request.headers.forEach((v, k) => { hdrs[k] = v; });
  const body = (request.method === 'GET' || request.method === 'HEAD') ? '' : await request.text();
  const out = JSON.parse(handleFetchJson(JSON.stringify({
    method: request.method,
    url: request.url,
    headers: hdrs,
    body
  })));
  return new Response(out.body, { status: out.status, headers: out.headers });
}
|}
         app_module
     in
     write_file (Filename.concat func_dir ".vc-config.json") vc_cfg;
     write_file (Filename.concat func_dir "index.js") js_code
   | `Native ->
     let vc_cfg =
       Json.to_pretty_string
         (Json.obj
            [
              ("runtime", Json.string "provided.al2023");
              ("handler", Json.string "bootstrap");
              ("maxDuration", Json.int 10);
            ])
     in
     write_file (Filename.concat func_dir ".vc-config.json") vc_cfg);
  out_dir

let emit_cloudflare_worker ?(out_dir = ".") ?(name = "quartz-edge-app") () =
  let wrangler =
    Printf.sprintf
      {|name = %S
main = "worker.js"
compatibility_date = "2026-01-01"

[assets]
directory = "./dist_site"
binding = "ASSETS"
|}
      name
  in
  let worker_js =
    {|import { handleFetchJson } from "./quartz_edge.bc.js";

export default {
  async fetch(request, env, ctx) {
    const hdrs = {};
    request.headers.forEach((v, k) => { hdrs[k] = v; });
    const body = (request.method === 'GET' || request.method === 'HEAD') ? '' : await request.text();
    const out = JSON.parse(handleFetchJson(JSON.stringify({
      method: request.method,
      url: request.url,
      headers: hdrs,
      body
    })));
    return new Response(out.body, { status: out.status, headers: out.headers });
  }
};
|}
  in
  write_file (Filename.concat out_dir "wrangler.toml") wrangler;
  write_file (Filename.concat out_dir "worker.js") worker_js;
  out_dir

let emit_docker_and_fly ?(out_dir = ".") ?(app_name = "quartz-app") ?(binary_name = "quartz") () =
  let dockerfile =
    Printf.sprintf
      {|# Multi-stage Alpine/musl production build for Quartz
FROM ocaml/opam:alpine-3.20-ocaml-5.2 AS builder
WORKDIR /app
RUN sudo apk add --no-cache musl-dev
COPY dune-project *.opam ./
RUN opam install . --deps-only --with-test -y
COPY . .
RUN opam exec -- dune build @install && opam exec -- dune runtest

FROM alpine:3.20
WORKDIR /srv
COPY --from=builder /app/_build/default/bin/main.exe /srv/quartz-server
COPY --from=builder /app/dist_site /srv/dist_site
EXPOSE 8080
ENV PORT=8080
CMD ["/srv/quartz-server", "serve", "8080"]
|}
  in
  let fly_toml =
    Printf.sprintf
      {|app = %S
primary_region = "fra"

[build]
  dockerfile = "Dockerfile"

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = true
  auto_start_machines = true
  min_machines_running = 1

[[vm]]
  size = "shared-cpu-1x"
|}
      app_name
  in
  ignore binary_name;
  write_file (Filename.concat out_dir "Dockerfile") dockerfile;
  write_file (Filename.concat out_dir "fly.toml") fly_toml;
  out_dir

let emit_mirage ?(out_dir = "mirage") ?(app_name = "quartz-unikernel") ?(static_dir = "dist_site") () =
  ensure_dir_p out_dir;
  write_file
    (Filename.concat out_dir "config.ml")
    (Quartz_mirage.Mirage_adapter.generate_config_ml ~app_name ~static_dir);
  write_file
    (Filename.concat out_dir "unikernel.ml")
    (Quartz_mirage.Mirage_adapter.generate_unikernel_ml ());
  out_dir

let generate_llms_txt ~title ~base_url (routes : Router.route_def list) : string =
  let route_lines =
    List.map
      (fun (r : Router.route_def) ->
        Printf.sprintf "- [%s](%s%s): `%s`"
          (Option.value ~default:r.pattern r.title)
          base_url r.pattern r.pattern)
      routes
  in
  Printf.sprintf
    {|# %s

> Built with Quartz -- Full-stack OCaml Web Framework (SSR, Signals, Headless WAI-ARIA UI, WCAG/APCA Audit, Scene3d WebGL2, MirageOS Unikernel & Edge targets).

## Routes
%s

## Quick CLI Commands
- `quartz check` -- Verify routes, links, WCAG/APCA color contrast & a11y
- `quartz build` -- Emit static HTML, CSS, hashed JS islands runtime & SEO sitemap
- `quartz dev`   -- Run multi-threaded HTTP/1.1 + LiveView + HMR server
|}
    title (String.concat "\n" route_lines)

let scaffold_project ~dir ~(template : [ `Saas | `Docs | `Api ]) =
  let name = Filename.basename dir in
  ensure_dir_p (Filename.concat dir "bin");
  ensure_dir_p (Filename.concat dir "public");
  write_file (Filename.concat dir "dune-project")
    (Printf.sprintf "(lang dune 2.9)\n(name %s)\n(version 0.1.0)\n" name);
  write_file (Filename.concat dir "bin/dune")
    "(executable\n (name main)\n (libraries quartz))\n";
  let main_code =
    match template with
    | `Api ->
      {ml|open Quartz

let () =
  api_get "/api/health" (fun _ -> {|{"status":"ok"}|});
  use_middleware (Middleware.cors ());
  use_middleware (Middleware.security_headers ());
  use_middleware (Middleware.req_timer ());
  run_browser ~port:8080 ~title:"Quartz API Service" (fun () ->
    div ~style:Style.[ p 32; flex_col; gap 12 ] [
      h1 ~style:Style.[ text_2xl; bold ] [ text "Quartz API Service" ];
      p [ text "GET /api/health is live." ];
    ])
|ml}
    | `Docs ->
      {|open Quartz

let routes = [
  route ~title:"Home" "/" (fun _ ->
    div ~style:Style.[ p 32; max_w 760; mx_auto; flex_col; gap 16 ] [
      h1 ~style:Style.[ text_3xl; bold ] [ text "Quartz Docs Template" ];
      Headless.accordion_item ~open_:true
        ~summary_style:Style.[ font_bold; cursor_pointer; color Palette.sky_400 ]
        ~panel_style:Style.[ pt 8; color Palette.slate_300 ]
        ~summary_kids:[ text "Zero-JS Progressive Accordion" ]
        [ text "Works even on static GitHub Pages builds." ];
    ]);
]

let () =
  let app () = Router.router routes in
  run_vite_check ~routes ~sample_paths:[ "/" ] app;
  run_browser ~port:8080 ~title:"Docs Site" app
|}
    | `Saas ->
      {|open Quartz

let counter = use_signal 1

let home _ =
  div ~style:Style.[ p 32; max_w 960; mx_auto; flex_col; gap 20 ] [
    badge ~variant:Primary "QUARTZ v4.0 SAAS STARTER";
    h1 ~style:Style.[ text_3xl; bold ] [ text "Ship OCaml Web Apps Everywhere" ];
    div ~style:Style.[ flex_row; gap 12; items_center ] [
      btn ~on_click:(fun () -> Signal.update counter (fun n -> n + 1)) "+1 Increment";
      rx_text (fun () -> Printf.sprintf "Count: %d" !~counter);
    ];
    Dom.canvas3d ~width:480 ~height:220 (Scene3d.create [ Scene3d.mesh () ]);
  ]

let routes = [ route ~title:"Home" "/" home ]

let () =
  let app () = Router.router routes in
  run_vite_check ~routes ~sample_paths:[ "/" ] app;
  run_browser ~port:8080 ~title:"Quartz SaaS Starter" app
|}
  in
  write_file (Filename.concat dir "bin/main.ml") main_code;
  dir
