# Quartz

Quartz is a full-stack web framework for OCaml (`>= 4.13` and `5.x`). It lets you write reactive UI components, server-rendered HTML, parameterized SQL queries, WebSocket/SSE streams, and 3D WebGL2/WebGPU scenes in a single type-checked OCaml codebase without maintaining a separate JavaScript build pipeline.

---

## Why Quartz Exists

Most web projects in OCaml stall for one of two reasons:
1. You have to stitch together a dozen separate libraries for routing, HTML templating, forms, sessions, WebSockets, and CSS, or
2. You hit a wall when you need a third-party JavaScript widget (Stripe, Chart.js, Monaco) or a standard SQLite/Postgres database and end up writing brittle boilerplate bindings.

Quartz takes a practical approach:
- **Pure Core (`quartz.core`):** The reactive signal graph, HTML/CSS compiler, router, form validator, WCAG/APCA contrast math, cryptographic primitives (SHA-256, HMAC, PBKDF2), and SQL query/schema builder have **zero `Unix` or C dependencies**. They compile anywhere—native Linux/macOS/Windows servers, MirageOS unikernels, or serverless edge workers.
- **Native Server & Tooling (`quartz.unix`):** A multi-threaded HTTP/1.1 + WebSocket (RFC 6455) + Server-Sent Events server, crash-safe Write-Ahead Log (WAL), multi-backend database driver (`node:sqlite`, PostgreSQL, LibSQL/Turso Hrana HTTP, Redis RESP2), static site bundler, and a 6-stage accessibility/contrast auditor.
- **Universal JS/NPM Plug (`Npm_plug` & `Npm_server_plug`):** Instead of hand-porting 2.5 million npm packages to OCaml, Quartz mounts browser packages directly via native ESM importmaps (`esm.sh`) and calls server-side Node/Bun packages over a persistent NDJSON stdio worker pool.

---

## Repository Layout

The codebase is organized by domain so you can find any subsystem in seconds:

```text
quartz/
├── bin/
│   └── main.ml                        # CLI entry point (`quartz dev`, `check`, `build`, `create`, `deploy`)
├── examples/
│   ├── counter_and_form.ml            # Minimal 25-line reactive counter + TUI demo
│   └── starter_website.ml             # Complete out-of-the-box multi-page website + SQLite changelog
├── lib/
│   ├── core/                          # Pure OCaml core (`quartz.core` — zero Unix/C deps)
│   │   ├── reactivity/                # signal.ml, resource.ml, debug_registry.ml
│   │   ├── ui/                        # dom.ml, style.ml, contrast.ml, headless.ml, components.ml, charts.ml, scene3d.ml, tui.ml
│   │   ├── web/                       # http.ml, router.ml, form.ml, middleware.ml, cookie.ml, multipart.ml, websocket.ml, pubsub.ml, isr.ml, app.ml
│   │   ├── data/                      # io.ml, json.ml, crypto_util.ml, i18n.ml, store.ml, sql.ml, jobs.ml
│   │   └── bridge/                    # npm_plug.ml
│   ├── runtime/                       # Target-specific execution engines
│   │   ├── unix/                      # `quartz.unix`
│   │   │   ├── server/                # server.ml, unix_adapters.ml
│   │   │   ├── db/                    # db_driver.ml (SQLite, Postgres, LibSQL/Turso, Redis RESP2, Embedded WAL)
│   │   │   ├── bridge/                # npm_server_plug.ml, import_dts.ml, import_html.ml
│   │   │   └── tooling/               # vite.ml, deploy.ml, scaffolder.ml
│   │   ├── browser/                   # browser_runtime.ml (keyed DOM patcher, WebGL2/WebGPU, WAI-ARIA, ESM loader)
│   │   ├── edge/                      # edge_adapter.ml (Cloudflare Workers / WinterCG)
│   │   ├── mirage/                    # mirage_adapter.ml (MirageOS 4 unikernel functor)
│   │   └── vercel/                    # vercel_runtime.ml (Vercel Build Output v3)
│   ├── adapters/                      # Optional opam ecosystem bridges
│   │   ├── lwt/                       # quartz.lwt (Io.CONCURRENCY over Lwt)
│   │   ├── re/                        # quartz.re (Form regex hook over re.pcre)
│   │   ├── uutf/                      # quartz.intl_uutf (Unicode normalization via uutf + uucp)
│   │   └── std/                       # quartz.std_adapters (Yojson AST bridge + Logs reporter)
│   └── facade/
│       └── quartz.ml                  # Single `open Quartz` ergonomic module
└── test/                              # Unit and integration test suites (200+ assertions)
```

---

## Quickstart

### 1. Build and Run the Test Suite

```bash
opam install . --deps-only -y
opam exec -- dune build @all
opam exec -- dune runtest --force
```

### 2. Run the Out-of-the-Box Starter Website

[`examples/starter_website.ml`](examples/starter_website.ml) is a complete, multi-page engineering website with a live SQLite/WAL release changelog, interactive throughput/cost sizing calculator, searchable documentation, and a 3D WebGL2/WebGPU sandbox.

```bash
# Run the 6-stage accessibility/WCAG auditor and export static HTML to dist_starter_site/
opam exec -- dune exec examples/starter_website.exe

# Or start the live multi-threaded HTTP/1.1 + WebSocket + SSE server on port 3000
opam exec -- dune exec examples/starter_website.exe -- serve 3000
```

### 3. Scaffold a New Project (Vite / Node Style Templates)

Quartz ships with 6 built-in project builders so you can start a fresh project with one command:

```bash
# List all available starter templates
opam exec -- dune exec quartz -- templates

# Create a new project from a template
opam exec -- dune exec quartz -- create fullstack-saas ./my-saas-app
```

| Template ID | Stack & Features |
| :--- | :--- |
| `minimal` | Reactive Signals, validated Form fields, SSR + keyed DOM patching |
| `fullstack-saas` | Parameterized SQL (`node:sqlite` / WAL), task CRUD dashboard, signed sessions |
| `static-blog` | Zero-JS-by-default blog & docs, `sitemap.xml`, `llms.txt`, GitHub Pages workflow |
| `realtime-collab` | RFC 6455 WebSocket rooms, PubSub topics, and live SSE event stream |
| `webgl-3d` | Declarative 3D meshes (`Torus`, `Sphere`, `Box`), WebGL2/WebGPU shaders, RK4 springs |
| `js-hybrid` | Client ESM widgets (`Npm_plug`) + server-side Node worker IPC (`Npm_server_plug`) |

---

## Core Engineering Patterns

### Reactive Signals & Type-Safe Routing

```ocaml
open Quartz

let replicas = use_signal 3
let total_rps = use_memo (fun () -> !~replicas * 4500)

let home_page _params =
  div ~style:Style.[ p 24; flex_col; gap 12 ] [
    h1 ~style:Style.[ text_2xl; bold ] [ text "Cluster Control Plane" ];
    rx (fun () ->
      p [ textf "Replicas: %d | Capacity: %d req/s" !~replicas !~total_rps ]);
    btn ~variant:Primary
      ~on_click:(fun () -> Signal.update replicas (fun n -> n + 1))
      "+ Scale Up";
  ]

(* Compile-time typed GADT route: /cluster/:region/:shard_id *)
let routes = [
  route ~title:"Home" "/" home_page;
  Router.gadt_route ~title:"Shard"
    Router.Path.(s "cluster" @@ str @@ int nil)
    (fun region shard_id ->
      div [ textf "Region: %s | Shard #%d" region shard_id ]);
]

let () =
  run_browser ~port:8080 ~title:"Control Plane" ~routes (fun () -> Router.router routes)
```

### Parameterized SQL & Multi-Backend Database Driver

`Quartz.Sql` compiles `?` and `$1..$n` placeholders across SQLite, PostgreSQL, and MySQL dialects with full SQL-injection protection. `Quartz.Db_driver` executes queries against Node 22's built-in `node:sqlite`, `better-sqlite3`, `pg` (PostgreSQL), LibSQL/Turso Hrana v2 HTTP pipelines, Redis RESP2 sockets, or the pure-OCaml crash-safe WAL engine.

```ocaml
open Quartz

let db = Db_driver.connect (Db_driver.Node_sqlite "production.db")

let () =
  ignore (Db_driver.migrate db [
    (1, "init_users",
     "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, email TEXT NOT NULL, role TEXT NOT NULL);");
  ]);
  ignore (Db_driver.exec db
    (Sql.q "INSERT INTO users (email, role) VALUES (?, ?)"
      [ Sql.Text "ada@ocaml.org"; Sql.Text "admin" ]));

  match Db_driver.query db (Sql.q "SELECT * FROM users WHERE role = ?" [ Sql.Text "admin" ]) with
  | Ok rows ->
    List.iter (fun r ->
      Printf.printf "Admin: %s\n" (Option.value (Sql.get_string "email" r) ~default:"")
    ) rows
  | Error err -> prerr_endline err
```

### Using NPM Libraries Without Writing FFI Ports

1. **Client-Side Browser Widgets (`Quartz.Npm_plug`):**
   ```ocaml
   let chart =
     Npm_plug.component
       ~pkg:"chart.js"
       ~version:"4.4.1"
       ~export_name:"Chart"
       ~framework:`Vanilla
       ~props:[ ("type", Json.string "line") ]
       ()
   ```
2. **Server-Side Node/Bun Worker Calls (`Quartz.Npm_server_plug`):**
   ```ocaml
   let digest =
     Npm_server_plug.eval_js
       ~code:"(args, require) => require('crypto').createHash('sha256').update(args).digest('hex')"
       ~args:(Json.string "payload")
   ```
3. **Auto-Generating Typed OCaml Wrappers from TypeScript `.d.ts`:**
   ```bash
   opam exec -- dune exec quartz -- gen-plug stripe stripe.d.ts > stripe_plug.ml
   ```

---

## Deployment Targets

Generate production configs and standalone Edge/Serverless bundles (`quartz_edge.bc.js`) with `quartz deploy <target> [out_dir]`:

```bash
opam exec -- dune exec quartz -- deploy pages .          # GitHub Pages (.github/workflows/pages.yml)
opam exec -- dune exec quartz -- deploy ci .             # GitHub Actions CI (.github/workflows/ci.yml)
opam exec -- dune exec quartz -- deploy docker .         # Multi-stage Alpine/musl Dockerfile + fly.toml
opam exec -- dune exec quartz -- deploy caddy .          # Caddyfile for automatic HTTP/2, HTTP/3 & Let's Encrypt TLS
opam exec -- dune exec quartz -- deploy cloudflare ./cf  # Cloudflare Worker (wrangler.toml + worker.js + quartz_edge.bc.js)
opam exec -- dune exec quartz -- deploy vercel ./vc      # Vercel Build Output API v3 (Edge / Node / Native)
opam exec -- dune exec quartz -- deploy deno ./deno      # Deno Deploy (deno.json + main.ts + quartz_edge.bc.js)
opam exec -- dune exec quartz -- deploy bun ./bun        # Bun.serve + bun:sqlite (server.ts + quartz_edge.bc.js)
opam exec -- dune exec quartz -- deploy lambda ./sam     # AWS Lambda Custom Runtime (template.yaml)
opam exec -- dune exec quartz -- deploy mirage ./mirage  # MirageOS 4 Unikernel (config.ml + unikernel.ml)
```

---

## License

MIT License. See [LICENSE](LICENSE) for details. Cause ain't noone paying us to do ts
