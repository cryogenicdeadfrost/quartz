# Quartz -- Version Tracker & Subsystem Roadmap

This file tracks shipped releases of **Quartz**, the reorganized module layout under `lib/`, and the pure-OCaml + ecosystem-bridge architecture across v1.0 through v4.2.2.

---

## Multi-Package Architecture (`lib/`)

```text
lib/
|-- core/                          # `quartz.core`: 100% pure stdlib OCaml (zero Unix, zero Threads)
|   |-- reactivity/                # signal.ml, resource.ml, debug_registry.ml (Chrome DevTools + Inspector)
|   |-- ui/                        # dom.ml, style.ml, contrast.ml, headless.ml, components.ml, charts.ml, scene3d.ml, tui.ml, custom_renderer.ml
|   |-- web/                       # http.ml, router.ml, form.ml, middleware.ml, cookie.ml, multipart.ml, websocket.ml, pubsub.ml, isr.ml, app.ml
|   |-- data/                      # io.ml, json.ml, crypto_util.ml, i18n.ml, store.ml, sql.ml, jobs.ml
|   `-- bridge/                    # npm_plug.ml (Universal ESM/React/WebComponent client adapter)
|-- runtime/                       # Target-specific execution engines
|   |-- unix/                      # `quartz.unix`: Multi-threaded HTTP/1.1 + WS + SSE runtime
|   |   |-- server/                # server.ml, unix_adapters.ml (Worker_pool, File_storage WAL, /__quartz/inspector)
|   |   |-- db/                    # db_driver.ml (Node_sqlite, Postgres, LibSQL/Turso Hrana HTTP, Redis RESP2, Embedded WAL)
|   |   |-- bridge/                # npm_server_plug.ml (Node/Bun/Deno NDJSON stdio worker), import_dts.ml, import_html.ml
|   |   `-- tooling/               # vite.ml (bundler + 6-stage check + HMR), deploy.ml, scaffolder.ml (6 project templates)
|   |-- browser/                   # `quartz.browser`: Keyed DOM patcher, FLIP, WebGL2 & WebGPU WGSL shaders, focus trap, esm.sh loader
|   |-- edge/                      # `quartz.edge`: WinterCG Fetch adapter (Cloudflare Workers, Vercel Edge, Deno, Bun)
|   |-- mirage/                    # `quartz.mirage`: MirageOS 4 Unikernel adapter + config.ml / unikernel.ml generators
|   `-- vercel/                    # `quartz.vercel`: Vercel Build Output API v3 + AWS Lambda bootstrap loop
|-- adapters/                      # Optional opam ecosystem bridges
|   |-- lwt/                       # `quartz.lwt`: Io.CONCURRENCY over Lwt
|   |-- re/                        # `quartz.re`: Form regex validator hook over re.pcre
|   |-- uutf/                      # `quartz.intl_uutf`: Unicode NFC/NFKC/word-count hook over uutf + uucp
|   `-- std/                       # `quartz.std_adapters`: Yojson AST bridge + Logs reporter
`-- facade/                        # `quartz`: Top-level public API preserving full backward compatibility
```

---

## Shipped Versions

* **`v1.0.0` - `v3.0.0`:** Reactive Signals/Memos, Flexbox TUI, pure-SVG Charts, WCAG 2.1 luminance engine, Middleware pipeline, ISR/SSE, `Custom_renderer.Make` (Canvas2D, Markdown, JSON Scenegraph), and 5-stage `quartz check`.
* **`v4.0.0` - `v4.1.0` ("One Core, Many Targets"):**
  * **Zero-`Unix` Core Split & Universal Targets:** `quartz.core` runs identically over `quartz.unix`, `quartz.mirage`, `quartz.edge`, `quartz.vercel`, and `quartz.browser`.
  * **HTTP/1.1, Sessions & Real-Time:** Per-visitor HMAC-SHA256 signed cookie sessions, chunked transfer + `Range` 206, multipart magic-byte upload parser, RFC 6455 WebSocket `Rooms`, streaming SSR (`render_streaming_document`), and SSE.
  * **Data, Jobs & RPC:** Append-only on-disk WAL store, schema `Migrations`, retry-backoff `Jobs` + `/api/__cron/*` webhooks, end-to-end typed `Middleware.Endpoint` RPCs, and `X-Request-Id` tracing.
  * **Frontend, Headless A11y, Motion & 3D:** Keyed DOM diff patching, `[data-q-island]` partial hydration + shared hashed runtime chunk, GADT `Router.Path` + loaders + 301/404 status codes, WAI-ARIA `Headless` primitives (`dialog`, `popover`, `accordion`, `tabs`, `menu`, `combobox`, `listbox`, `tooltip`, `live_toast`) with roving tabindex + focus trap, OKLCH/APCA contrast auditing, RK4 spring `linear(...)`, FLIP `[data-q-flip]`, `@starting-style`, `@view-transition`, and declarative `Scene3d` (`WebGPU`/`WGSL`, `WebGL2`, `ThreeJS`).
* **`v4.2.0` - `v4.2.1` (Universal NPM Plugs, Opam Adapters & Cloud Parity):**
  * **Zero-Porting NPM Universal Plugs:** `Npm_plug` (dynamic client-side `esm.sh` loader for Vanilla, React, and Web Components), `Npm_server_plug` (persistent Node/Bun/Deno stdio NDJSON worker pool implementing `Io.PLUG_TRANSPORT`), and `Import_dts` (`quartz gen-plug` TypeScript `.d.ts` -> OCaml wrapper generator).
  * **Security, Diagnostics & Optional Opam Adapters:** RFC 6070 PBKDF2-HMAC-SHA256 (`Crypto_util`), Chrome DevTools custom formatter (`Debug_registry`) + `/__quartz/inspector`, responsive `Dom.cdn_image_url`, browser `Intl` hooks, and optional packages (`quartz.lwt`, `quartz.re`, `quartz.intl_uutf`, `quartz.std_adapters`).
  * **Multi-Cloud Deployment Generators:** One-command emitters for GitHub Pages, GitHub Actions CI, Vercel (Edge, Node 22, Custom Lambda), Cloudflare Workers, Deno Deploy, Bun, Netlify Edge, AWS SAM, Fly.io, Docker/musl, Railway, Render, Caddy, and MirageOS 4.
* **`v4.2.2` (Database Drivers, Reorganized Codebase, WebGPU Shaders & Starter Builders):**
  * **Parameterized SQL & Multi-Backend DB Drivers:** Pure-OCaml `Quartz.Sql` dialect compiler (`?` <-> `$1..$n`, DDL generator, embedded relational + transaction engine) and `Quartz.Db_driver` supporting Node 22 `node:sqlite`, `better-sqlite3`, PostgreSQL (`pg`), Turso/LibSQL/D1 Hrana v2 HTTP pipelines, Redis RESP2 sockets, and crash-safe WAL persistence.
  * **Complete Starter Experience:** Out-of-the-box multi-page website (`examples/starter_website.ml`), 6 Vite/Node-style project templates via `quartz templates` and `quartz create <template> <dir>` (`minimal`, `fullstack-saas`, `static-blog`, `realtime-collab`, `webgl-3d`, `js-hybrid`), standalone `quartz_edge.bc.js` edge bundle generation, `.nojekyll` + `404.html` GitHub Pages routing, and full WGSL vertex/fragment mesh pipeline for `Scene3d` WebGPU canvases.

---

## Ecosystem Boundary & Adapter Model

External protocol, TLS, image-processing, and native C libraries (`ocaml-tls`, `letsencrypt`, `argon2`, `caqti`/`pgx`, `irmin`, `eio`, `h2`/`paf`, `js_of_ocaml`/`brr`, `libvips`) are cleanly decoupled behind `Io.CLOCK`, `Io.STORAGE`, `Io.PUBSUB`, `Io.SIGNER`, `Io.PLUG_TRANSPORT`, and optional opam packages under `lib/adapters/` so `quartz` compiles cleanly with zero mandatory third-party opam dependencies while integrating with npm, SQLite/Postgres/Redis, and opam stacks when needed.
