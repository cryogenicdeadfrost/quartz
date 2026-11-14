# Changelog

## 4.0.0 (2026-11-15) -- "One Core, Many Targets"
* **Multi-Package Split (`lib/`):**
  * **`quartz.core`** (`lib/core_pkg/`): 100% pure OCaml stdlib core with zero `Unix` and zero `Threads` dependencies (`Io`, `Json` RFC 8259 + JSONPath combinators, `Crypto_util` FIPS SHA-256/HMAC-SHA256/SHA-1/Base64, `Cookie` signed cookies + per-visitor `Session.Make`, `I18n` UTF-8/CLDR plurals/UTC RFC 7231 dates, `Signal`, `Resource`, `Contrast` OKLCH + APCA Lc + WCAG 2.1, `Style` RK4 spring physics + scroll/view timelines + `@keyframes`/`@property`/container queries, `Scene3d` declarative WebGL2 AST, `Dom` keyed snapshot diffing + islands + SEO meta, `Headless` WAI-ARIA primitives, `Router` GADT typed paths + 301/404 status resolution, `Form` honeypot + validation, `Charts`, `Components`, `Middleware` typed RPC `Endpoint` + ETag 304 + body limits + Guards/Interceptors, `Isr`, `Pubsub`, `Websocket` RFC 6455 + topic `Rooms`, `Multipart` + MIME magic-byte sniffing, `Http` RFC 9112 chunked/keep-alive/`Range` 206 parser, `Store` WAL + schema `Migrations`, `Jobs` retry queue + cron webhooks, `Tui`, `Custom_renderer`, and universal `App.Make` functor).
  * **`quartz.browser`** (`lib/browser_pkg/`): Shared runtime JS (`client_runtime_js`) for keyed DOM patching, `[data-q-island]` hydration, 60fps WebGL2 `Scene3d` renderer, WAI-ARIA roving tabindex, hover prefetching, HMR client, and `Js_bridge` external widget mounts.
  * **`quartz.edge`** (`lib/edge_pkg/`): WinterCG Fetch adapter (`Edge_adapter.Make`) for Vercel Edge, Cloudflare Workers, Deno Deploy, and Bun.
  * **`quartz.mirage`** (`lib/mirage_pkg/`): MirageOS 4 unikernel adapter (`Mirage_adapter.Make`) + `config.ml` / `unikernel.ml` generators.
  * **`quartz.vercel`** (`lib/vercel_pkg/`): AWS Lambda Runtime HTTP API (`bootstrap`) loop for native static Linux binaries.
  * **`quartz.unix`** (`lib/unix_pkg/`): Multi-threaded keep-alive HTTP/1.1 + WebSocket + SSE server, crash-safe on-disk WAL `File_storage`, `Vite` 6-stage doctor (routes, page budget, WCAG + APCA, a11y, API/ISR, socket probe), hashed JS runtime bundler + `llms.txt`, `Unix.stat` HMR watcher, `Deploy` generators (`pages`, `vercel`, `cloudflare`, `fly`, `mirage`, `new` templates), and `Import_html` HTML/Tailwind -> OCaml codegen.
  * **`quartz`** (`lib/facade/`): Full backward-compatible top-level facade (`open Quartz`).
* **CLI & Tests:** Added `deploy`, `import-html`, `new`, `dev --hmr`, `--help`, and `test_quartz_v4` (77 assertions across 11 groups alongside the v3 suite).

## 3.0.0 (2026-11-14)
* **Subsystem Folder Architecture (`lib/`):**
  * `lib/core/`: `Signal` (with `lens` & `make_history`) + `Resource` (Async `<Suspense>` resources, SWR cache, and `mutate_optimistic` with automatic rollback).
  * `lib/style/`: `Style` & `Palette` + `Contrast` (WCAG 2.1 sRGB gamma-linearized Relative Luminance $L$, `contrast_ratio`, `passes_aa`, `passes_aaa`, `pick_ink` / `auto_text_color`, `mix`, `tint`, `shade`).
  * `lib/frontend/`: `Dom` (Declarative Shadow DOM `shadow_root`, `custom_element`, `view_transition`, `suspense`), `Router`, `Form`, `Charts` (added `radar_chart` & `candlestick_chart`), `Components` (added `kanban_board`, `command_palette`, `timeline`, `stepper`, `avatar`, `skeleton`, `contrast_badge`).
  * `lib/backend/`: `Middleware` (Express/NestJS-style composable pipeline with `cors`, token-bucket `rate_cop`, `bearer_guard`, `csrf_shield`, `security_headers`, `req_timer`), `Isr` (Incremental Static Regeneration cache), `Pubsub` (Topic bus & `/__quartz_sse` Server-Sent Events), and `Server`.
  * `lib/renderers/`: `Tui` (Flexbox ANSI Terminal renderer) + `Custom_renderer` (`Make` functor + `render_markdown`, `render_canvas2d` GPU draw-call buffer, and `render_json_scene` mobile/native AST).
  * `lib/tooling/`: `Vite` (`run_vite_check` 5-stage doctor with WCAG contrast auditing & page weight budgets + `run_vite_build` emitting `sitemap.xml`, `robots.txt`, and `build-manifest.json`).

## 2.0.0 (2026-11-14)
* Added 60fps cursor spotlight tracking (`spotlight_card`), `area_chart`, `donut_ring`, `multi_line_chart`, `heatmap`, time-travel undo/redo signals, nested router layouts, and initial Vite-style `quartz check` / `quartz build` CLI.

## 1.0.0 (2026-11-14)
* Initial release of `Quartz` reactive UI, Tailwind-style CSS compiler, LiveView server, and Flexbox TUI renderer.
