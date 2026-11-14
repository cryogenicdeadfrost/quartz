# Quartz

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![npm required](https://img.shields.io/badge/node__modules-0%20bytes-brightgreen.svg)](#)

**Write full-stack reactive UIs in OCaml like it's Next.js + Svelte + Tailwind + Express, without touching `npm`, `webpack`, or a 900MB `node_modules` folder.**

Existing OCaml web/UI setups usually make you choose between two kinds of pain:

1. Set up a whole cross-compiler JS bundler pipeline that breaks every time someone sneezes on `package.json`.
2. Write raw string-concatenated HTML templates like it's 1999.

**`Quartz`** takes a different route: it gives you **React/Svelte-style reactive signals & `<Suspense>` resources**, **Tailwind-style type-safe styling with built-in WCAG 2.1 Color Contrast math & 60fps fluid motion**, an **Express/NestJS-style functional Middleware pipeline + ISR cache + SSE live push**, **pure-SVG data visualizations**, and a **pluggable Custom Renderer Engine** that compiles the exact same OCaml view tree into:

1. **Interactive Browser LiveView + REST + SSE Server** (`Quartz.run_browser`)
2. **Multi-Page Static HTML/CSS + SEO Sitemap Bundle** (`Quartz.run_vite_build`)
3. **Flexbox Terminal UI** (`Quartz.run_terminal` / `quartz tui`)
4. **HTML5 Canvas2D GPU Draw-Call Buffer** (`Quartz.render_canvas2d` / `quartz canvas`)
5. **GitHub-Flavored Markdown Document** (`Quartz.render_markdown` / `quartz markdown`)
6. **Native Mobile / Game-Engine JSON Scenegraph** (`Quartz.render_json_scene` / `quartz json`)

---

## Quick Honesty Note

Just keeping it 100% real: I designed `Quartz` because I wanted building websites and dashboards in OCaml to feel as effortless as TypeScript + React/Svelte, and **I used AI to help validate the edge cases and layout/luminance math** (like WCAG 2.1 sRGB gamma linearization and making sure exceptions inside a signal computation don't permanently brick the observer stack). Every subsystem is tested end-to-end with `dune runtest`. And honestly waaaay cooler to do this.

---

## Clean Subsystem Folder Architecture

All subsystems live in separate folders under `lib/` while remaining 100% accessible via a single `open Quartz`:

* **`lib/core/`** — [signal.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/core/signal.ml) (Signals, memos, batching, lenses, time-travel `undo`/`redo`) & [resource.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/core/resource.ml) (Async `<Suspense>` resources, SWR cache, optimistic mutations with automatic rollback)
* **`lib/style/`** — [style.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/style/style.ml) (Flexbox/Grid, glassmorphism, spring physics, media queries) & [contrast.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/style/contrast.ml) (WCAG 2.1 relative luminance $L$, contrast ratio $CR$, `pick_ink` auto-text color, `passes_aa`/`passes_aaa`)
* **`lib/frontend/`** — [dom.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/frontend/dom.ml) (HTML5/SVG, Declarative Shadow DOM, Custom Elements, View Transitions), [router.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/frontend/router.ml), [form.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/frontend/form.ml), [charts.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/frontend/charts.ml) (`area_chart`, `donut_ring`, `multi_line_chart`, `radar_chart`, `candlestick_chart`, `heatmap`), [components.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/frontend/components.ml) (`spotlight_card`, `kanban_board`, `command_palette`, `timeline`, `stepper`, `drawer`, `modal`, `accordion`, `switch_toggle`, `slider`, `toast_rack`)
* **`lib/backend/`** — [middleware.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/backend/middleware.ml) (`cors`, token-bucket `rate_cop`, `bearer_guard`, `csrf_shield`, `security_headers`, `req_timer`), [isr.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/backend/isr.ml) (Incremental Static Regeneration cache), [pubsub.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/backend/pubsub.ml) (SSE live broadcast bus), [server.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/backend/server.ml)
* **`lib/renderers/`** — [tui.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/renderers/tui.ml) (Flexbox ANSI Terminal renderer) & [custom_renderer.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/renderers/custom_renderer.ml) (`Custom_renderer.Make` functor + `Canvas2d`, `Markdown`, and `Json_scene` renderers)
* **`lib/tooling/`** — [vite.ml](file:///a:/Projects/OCaml_OpenSource/quartz/lib/tooling/vite.ml) (5-stage `quartz check` doctor with dead-link & WCAG contrast auditing + `quartz build` static bundler with `sitemap.xml` & `build-manifest.json`)

See [VERSIONS_AND_ROADMAP.md](file:///a:/Projects/OCaml_OpenSource/quartz/VERSIONS_AND_ROADMAP.md) and [docs/ECOSYSTEM_COMPARISON.md](file:///a:/Projects/OCaml_OpenSource/quartz/docs/ECOSYSTEM_COMPARISON.md) for the full feature matrix vs. React, SvelteKit, Next.js, Nuxt, Express, NestJS, Vite, Node, and Bun.

---

## Build & Run

```bash
git clone https://github.com/cryogenicdeadfrost/quartz.git
cd quartz

# Build all 6 subsystems & CLI
dune build

# Run the unit & integration test suite across all subsystems
dune runtest

# Run the 5-stage Vite-style Doctor, WCAG Contrast Auditor & Link Checker
dune exec quartz -- check

# Build the multi-page static site + sitemap.xml + build-manifest.json to ./dist_site
dune exec quartz -- build

# Render the exact same UI tree across Custom Renderers:
dune exec quartz -- tui        # Flexbox Terminal UI
dune exec quartz -- markdown   # GitHub-Flavored Markdown
dune exec quartz -- canvas     # HTML5 Canvas2D GPU Draw Calls
dune exec quartz -- json       # Native JSON Scenegraph AST

# Start the interactive LiveView + Middleware + REST + SSE server on http://127.0.0.1:8080
dune exec quartz -- dev 8080
```

---

## License

MIT cause ain't noone paying for ts.
