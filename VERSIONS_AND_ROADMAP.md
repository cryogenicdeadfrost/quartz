# Quartz -- Version Tracker & Subsystem Roadmap

This file tracks every version iteration of **Quartz**, how the codebase is split into modular subsystems under `lib/`, and which features from the JS/TS ecosystem (React, SvelteKit, Next.js, Nuxt, Express, NestJS, Vite, Node, Bun)--plus OCaml-exclusive capabilities--are shipped in each release.

---

## Subsystem Folder Layout (`lib/`)

```text
lib/
|-- dune                        # (include_subdirs unqualified)
|-- quartz.ml                   # Top-level ergonomic facade (`open Quartz`)
|-- core/                       # Reactive Signals, Stores, Undo/Redo & Async Suspense
|   |-- signal.ml               # Fine-grained Signals, computed memos, batching, time-travel
|   `-- resource.ml             # Async Suspense resources, SWR cache, optimistic mutations
|-- style/                      # Type-Safe CSS, Themes & WCAG 2.1 Color Contrast Math
|   |-- style.ml                # Flexbox/Grid, glassmorphism, spring physics, media queries
|   `-- contrast.ml             # sRGB relative luminance, WCAG AA/AAA contrast math, pick_ink
|-- frontend/                   # DOM, Custom Elements, Router, Forms, SVG Charts & UI Kit
|   |-- dom.ml                  # HTML5/SVG AST, Shadow DOM, Custom Elements, View Transitions
|   |-- router.ml               # Nested layouts, guards, query state, breadcrumbs, link auditor
|   |-- form.ml                 # Combinator field validators & auto-styled inputs
|   |-- charts.ml               # Sparkline, Area, Bar, Donut, Multi-Line, Heatmap, Radar, Candle
|   `-- components.ml           # Spotlight cards, Kanban, Command Palette, Timeline, Stepper, etc.
|-- backend/                    # Express/NestJS Middleware, ISR Cache, PubSub SSE & Server
|   |-- middleware.ml           # Functional middleware pipeline (CORS, Token-Bucket Rate Limit, Auth, CSRF)
|   |-- isr.ml                  # Next.js-style Incremental Static Regeneration (TTL cache)
|   |-- pubsub.ml               # Topic broadcast bus & Server-Sent Events (`/__quartz_sse`) framing
|   `-- server.ml               # Native HTTP/1.1 + LiveView + JSON REST + SSE + Middleware runner
|-- renderers/                  # Multi-Target Custom Renderers (TUI, Canvas2D, Markdown, JSON Wire)
|   |-- tui.ml                  # Pure-OCaml Flexbox layout solver & ANSI Terminal renderer
|   `-- custom_renderer.ml      # Functor (`Make`) + Canvas2D GPU command buffer, Markdown & JSON AST
`-- tooling/                    # Vite-Style Doctor, WCAG Auditor, Budget Checker & Static Bundler
    `-- vite.ml                 # `quartz check` & `quartz build` with sitemap.xml & manifest output
```

---

## Version History & Feature Matrix

### `v1.0.0` -- Initial Reactive Core & Dual Renderer (Shipped)
* **Core:** `Signal` (`create`, `computed`, `watch`, `batch`, `create_store`)
* **Styling:** `Style` & `Palette` (Tailwind-style OCaml rules, `hover`, `focus`, `dark`, responsive `@media`)
* **Frontend:** `Dom`, `Router` (`:param` matching), `Form` validators, `Components` (`card`, `modal`, `tabs`, `data_table`, `sparkline`, `bar_chart`)
* **Renderers & Server:** `Tui` Flexbox terminal renderer + `Server` LiveView HTTP server & static exporter

### `v2.0.0` -- 60fps Fluid Motion, Pure-SVG Chart Suite & Vite Doctor (Shipped)
* **Motion:** 60fps cursor-tracking radial `spotlight_card` (`--qx`, `--qy`), `hover_lift` spring physics, `glass` backdrop blur, keyframe animations, `on_mousemove` server coordinate streaming
* **Charts:** `area_chart` (linear gradients), `donut_ring`, `multi_line_chart`, `heatmap`
* **State & UI:** Time-travel `use_history` (`undo`/`redo`), `drawer`, `accordion`, `switch_toggle`, `slider`, `progress_bar`, `toast_rack`
* **Router & CLI:** `with_layout`, route `?guard`, `query_param`/`set_query`, `breadcrumbs`, `api_get`/`api_post`, and initial `run_vite_check` / `run_vite_build`

### `v2.1.0` -- Subsystem Modularization, WCAG 2.1 Color Contrast Math & Async Suspense
* **Folder Architecture:** Split `lib/` into `core/`, `style/`, `frontend/`, `backend/`, `renderers/`, and `tooling/`.
* **WCAG 2.1 Color Contrast Engine (`lib/style/contrast.ml`):**
  * Gamma-corrected sRGB relative luminance $L = 0.2126 R + 0.7152 G + 0.0722 B$
  * Contrast ratio $CR = (L_{\max} + 0.05) / (L_{\min} + 0.05)$
  * `pick_ink` / `auto_text_color` (automatically picks high-contrast light/dark foreground text for any background color)
  * `check_aa` ($\ge 4.5:1$) and `check_aaa` ($\ge 7.0:1$) + `mix` / `lighten` / `darken` helpers
* **Async Resources & `<Suspense>` (`lib/core/resource.ml`):**
  * `'a resource_state = Loading | Ready of 'a | Failed of string`
  * Optimistic mutations (`mutate_optimistic`) with automatic rollback on failure
  * `suspense` reactive boundary component that renders an animated shimmer `skeleton` while `Loading`

### `v2.2.0` -- Express/NestJS Middleware Pipeline, ISR Cache, SSE Live Push & Vite Bundler Pro
* **Functional Middleware Pipeline (`lib/backend/middleware.ml`):**
  * Composable `type middleware = handler -> handler` (`pipeline`)
  * `cors` (Access-Control headers), `rate_cop` ($O(1)$ token-bucket rate limiter), `bearer_guard` (Auth token verification), `csrf_shield`, `security_headers`, `req_timer`
* **Incremental Static Regeneration (`lib/backend/isr.ml`):**
  * Next.js-style in-memory TTL page cache (`get_or_render ~revalidate_sec`) with hit/miss/stale telemetry
* **PubSub & Server-Sent Events (`lib/backend/pubsub.ml`):**
  * Real-time topic bus (`publish`, `subscribe`, `sse_event`) wired to `/__quartz_sse`
* **Vite Tooling Upgrade (`lib/tooling/vite.ml`):**
  * Automatic DOM foreground/background **WCAG Contrast Ratio Auditing** inside `quartz check`
  * Page weight budget enforcement (`~max_kb`)
  * Automatic `sitemap.xml`, `robots.txt`, and `manifest.json` generation during `quartz build`

### `v3.0.0` -- Pluggable Custom Renderers (Canvas2D / Markdown / JSON) & Advanced UI Kit
* **Custom Renderer Engine (`lib/renderers/custom_renderer.ml`):**
  * First-class OCaml `Make_renderer` functor & custom backend vtable
  * **`Canvas2d_renderer`**: Compiles `Dom.node` trees into GPU Canvas2D draw call buffers (`fillRect`, `roundRect`, `fillText`, `strokePath`)
  * **`Markdown_renderer`**: Compiles `Dom.node` trees into clean GitHub-Flavored Markdown documents
  * **`Json_wire_renderer`**: Compiles `Dom.node` trees into compact JSON Scenegraphs for mobile/native clients
* **Web Components, Shadow DOM & View Transitions (`lib/frontend/dom.ml`):**
  * `custom_element`, `shadow_root` (`<template shadowrootmode="open">`), `document.startViewTransition` route morphing, and `data-q-prefetch` link prefetching
* **Advanced Interactive Components & Charts (`lib/frontend/components.ml`, `lib/frontend/charts.ml`):**
  * `kanban_board` (interactive multi-stage task board with live lane moves)
  * `command_palette` (`Ctrl+K` quick-action modal with instant search filtering)
  * `timeline` (vertical deployment/activity stream)
  * `stepper` (multi-phase workflow progress bar)
  * `avatar` & `avatar_group`, `skeleton` shimmer loaders
  * `radar_chart` (multi-axis spider SVG chart) & `candlestick_chart` (OHLC SVG chart)
