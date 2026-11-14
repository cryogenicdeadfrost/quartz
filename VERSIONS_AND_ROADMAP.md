# Quartz -- Version Tracker & Subsystem Roadmap

This file tracks every version iteration of **Quartz**, how the codebase is modularized into `quartz.core` + target adapters under `lib/`, and the pure-OCaml capabilities shipped through v4.0-v4.2.

---

## Multi-Package Architecture (`lib/`)

```text
lib/
|-- core_pkg/                   # `quartz.core`: 100% pure stdlib OCaml (zero Unix, zero Threads)
|   |-- io.ml                   # IO, CLOCK, STORAGE, PUBSUB, SIGNER signatures + Clock/Memory hooks
|   |-- json.ml                 # RFC 8259 JSON parser/serializer + JSONPath Decode combinators
|   |-- crypto_util.ml          # FIPS 180-4 SHA-256, RFC 2104 HMAC-SHA256, SHA-1, Base64/url
|   |-- cookie.ml               # Signed cookies + Cookie.Session.Make per-visitor manager
|   |-- i18n.ml                 # UTF-8 validation/counting, CLDR plural rules, UTC RFC 7231 dates
|   |-- signal.ml / resource.ml # Reactive Signals, computed memos, undo/redo, Suspense resources
|   |-- style.ml / contrast.ml  # OKLCH color scales, APCA Lc, WCAG 2.1, RK4 springs, @starting-style, @view-transition, CSS dedup
|   |-- scene3d.ml              # Declarative 3D AST (WebGPU/WGSL, WebGL2/GLSL, Three.js)
|   |-- dom.ml / headless.ml    # Keyed snapshot DOM diffing, Islands, SEO meta, WAI-ARIA Headless UI
|   |-- router.ml / form.ml     # GADT Router.Path, loaders, 301/404 resolution, Honeypot validation
|   |-- charts.ml / components.ml
|   |-- middleware.ml           # Typed RPC Endpoint, CORS, rate limiter, CSRF, ETag 304, body limits, request IDs
|   |-- isr.ml / pubsub.ml      # Incremental Static Regeneration + SSE PubSub bus
|   |-- websocket.ml            # RFC 6455 framing + topic Rooms
|   |-- multipart.ml / http.ml  # Streaming multipart + magic-byte sniffer + RFC 9112 chunked/Range 206 parser
|   |-- store.ml / jobs.ml      # Append-only WAL KV + schema Migrations + retry Jobs + cron webhooks
|   |-- tui.ml / custom_renderer.ml / app.ml  # Flexbox TUI, Custom_renderer.Make, App.Make functor
|-- browser_pkg/                # `quartz.browser`: Client islands, DOM patcher, FLIP, WebGPU/WebGL2, focus trap, HMR WS
|-- edge_pkg/                   # `quartz.edge`: WinterCG Fetch adapter (Vercel Edge, Cloudflare Workers, Deno, Bun)
|-- mirage_pkg/                 # `quartz.mirage`: MirageOS 4 Unikernel adapter + config.ml/unikernel.ml generators
|-- vercel_pkg/                 # `quartz.vercel`: AWS Lambda Runtime HTTP API (`bootstrap`) loop
|-- unix_pkg/                   # `quartz.unix`: Multi-threaded HTTP/1.1+WS+SSE, /__quartz/inspector, File WAL, Deploy, Import_html, Vite
`-- facade/                     # `quartz`: Top-level public API preserving full backward compatibility
```

---

## Shipped Versions

* **`v1.0.0` - `v3.0.0`:** Reactive Signals/Memos, Flexbox TUI, pure-SVG Charts, WCAG 2.1 luminance engine, Middleware pipeline, ISR/SSE, `Custom_renderer.Make` (Canvas2D, Markdown, JSON Scenegraph), and 5-stage `quartz check`.
* **`v4.0.0` - `v4.2` ("One Core, Many Targets"):**
  * **Zero-`Unix` Core Split & Universal Targets:** `quartz.core` runs identically over `quartz.unix`, `quartz.mirage`, `quartz.edge`, `quartz.vercel`, and `quartz.browser`.
  * **HTTP/1.1, Sessions & Real-Time:** Per-visitor HMAC-SHA256 signed cookie sessions, chunked transfer + `Range` 206, multipart magic-byte upload parser, RFC 6455 WebSocket `Rooms`, streaming SSE, and `/__quartz/inspector`.
  * **Data, Jobs & RPC:** Append-only on-disk WAL store, schema `Migrations`, retry-backoff `Jobs` + `/api/__cron/*` webhooks, end-to-end typed `Middleware.Endpoint` RPCs, and `X-Request-Id` tracing.
  * **Frontend, Headless A11y, Motion & 3D:** Keyed DOM diff patching, `[data-q-island]` partial hydration + shared hashed runtime chunk, GADT `Router.Path` + loaders + 301/404 status codes, WAI-ARIA `Headless` primitives (`dialog`, `popover`, `accordion`, `tabs`, `menu`, `combobox`, `listbox`, `tooltip`, `live_toast`) with roving tabindex + focus trap, OKLCH/APCA contrast auditing, RK4 spring `linear(...)`, FLIP `[data-q-flip]`, `@starting-style`, `@view-transition`, and declarative `Scene3d` (`WebGPU`/`WGSL`, `WebGL2`, `ThreeJS`).
  * **Tooling & One-Command Deploy:** Deduplicated hashed CSS/JS build output, `sitemap.xml`, `robots.txt`, `llms.txt`, `Unix.stat` HMR dev watcher, `quartz import-html`, `quartz new <saas|docs|api>`, and `quartz deploy <pages|ci|vercel|cloudflare|docker|fly|mirage>`.

---

## Optional Borrowed / External Ecosystem Boundaries

External C/opam/JS ecosystems from [docs/COMPLETION_PLAN.md](docs/COMPLETION_PLAN.md) (`ocaml-tls`, `letsencrypt`, `argon2`, `caqti`/`pgx`, `irmin`, `eio`, `h2`/`paf`, `js_of_ocaml`/`brr`, `libvips`/`imagemagick`) remain cleanly abstracted behind `Io.CLOCK`, `Io.STORAGE`, `Io.PUBSUB`, `Io.SIGNER`, and adapter functors so `quartz` builds and passes with zero external opam dependencies.
