# Quartz

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![npm required](https://img.shields.io/badge/node__modules-0%20bytes-brightgreen.svg)](#)

**One Pure OCaml Core, Many Targets.** Build full-stack reactive web applications, APIs, interactive dashboards, 3D scenes, and static sites in pure OCaml with zero `node_modules` -- and target **Unix servers, MirageOS unikernels, Vercel, Cloudflare Workers, Fly.io, GitHub Pages, Terminal UIs, Canvas2D, and Markdown** from a single codebase.

---

## Package Architecture (`v4.0.0`)

All subsystems preserve 100% `open Quartz` API compatibility while enforcing a strict zero-`Unix` boundary in `quartz.core`:

* **`quartz.core`** (`lib/core_pkg/`): Pure stdlib OCaml core (zero `Unix`, zero `Threads`). Includes `Signal`, `Resource`, `Json` (RFC 8259 + JSONPath combinators), `Crypto_util` (FIPS SHA-256, HMAC-SHA256, SHA-1, Base64), `Cookie` + per-visitor `Session.Make`, `I18n` (UTF-8, CLDR plurals, UTC HTTP dates), `Contrast` (OKLCH scales, APCA Lc, WCAG 2.1), `Style` (RK4 springs, `@keyframes`, `@property`, container queries, scroll timelines), `Scene3d` (WebGL2 AST), `Dom` (keyed diff patching, islands, SEO meta), `Headless` (WAI-ARIA primitives), `Router` (GADT typed routes + 301/404), `Form` (validation + honeypot), `Charts`, `Components`, `Middleware` (typed RPC `Endpoint`, ETag 304, body limits, guards/interceptors), `Isr`, `Pubsub`, `Websocket` (RFC 6455 + topic rooms), `Multipart` + MIME sniffer, `Http` (RFC 9112 chunked/keep-alive/`Range` 206), `Store` (WAL + migrations), `Jobs` (retries + cron webhooks), `Tui`, `Custom_renderer`, and `App.Make`.
* **`quartz.browser`** (`lib/browser_pkg/`): Shared runtime JS chunk (`client_runtime_js`) for DOM diffing, `[data-q-island]` hydration, 60fps WebGL2 `Scene3d` runner, keyboard roving tabindex, hover prefetching, and `Js_bridge` widgets.
* **`quartz.edge`** (`lib/edge_pkg/`): WinterCG Fetch JSON adapter (`Edge_adapter.Make`) for Vercel Edge, Cloudflare Workers, Deno Deploy, and Bun.
* **`quartz.mirage`** (`lib/mirage_pkg/`): MirageOS 4 unikernel functor (`Mirage_adapter.Make`) plus `config.ml` and `unikernel.ml` generators.
* **`quartz.vercel`** (`lib/vercel_pkg/`): AWS Lambda Runtime HTTP API (`bootstrap`) loop for native Linux binaries.
* **`quartz.unix`** (`lib/unix_pkg/`): Multi-threaded HTTP/1.1 + WebSocket + SSE server, on-disk WAL `File_storage`, `Vite` 6-stage doctor (routes, page budget, WCAG + APCA, a11y, API/ISR, socket probe), hashed asset bundler + `llms.txt`, `Unix.stat` HMR watcher, `Deploy` generators, `Import_html` codegen, and `new` project templates.
* **`quartz`** (`lib/facade/`): Top-level public facade re-exporting all v3 and v4 APIs.

---

## Build, Test & CLI

```bash
git clone https://github.com/cryogenicdeadfrost/quartz.git
cd quartz

# Build all packages & CLI
dune build

# Run full test suite (test_quartz + test_quartz_v4)
dune runtest --force

# Run the 6-stage Doctor (Routes, Budget, WCAG + APCA, WAI-ARIA, API/ISR, Socket)
dune exec quartz -- check

# Emit multi-page static HTML/CSS + hashed JS runtime + sitemap.xml + llms.txt
dune exec quartz -- build

# Start LiveView HTTP/1.1 + WebSocket + SSE dev server (add --hmr for hot reload)
dune exec quartz -- dev 8080 --hmr

# Generate cloud / unikernel deploy artifacts & starter scaffolds
dune exec quartz -- deploy vercel
dune exec quartz -- deploy cloudflare
dune exec quartz -- deploy fly
dune exec quartz -- deploy mirage
dune exec quartz -- deploy pages
dune exec quartz -- new saas my_app

# Convert HTML + Tailwind markup to Quartz OCaml code
dune exec quartz -- import-html snippet.html

# Alternate renderers for `/`
dune exec quartz -- tui
dune exec quartz -- markdown
dune exec quartz -- canvas
dune exec quartz -- json
```

---

## License

MIT -- see [LICENSE](LICENSE).
