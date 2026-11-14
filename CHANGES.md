# Changelog

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
