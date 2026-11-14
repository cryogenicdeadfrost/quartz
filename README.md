# Quartz

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![npm required](https://img.shields.io/badge/node__modules-0%20bytes-brightgreen.svg)](#)

**Write UI in OCaml like it's Next.js + Svelte + Tailwind, without touching `npm`, `webpack`, or a 900MB `node_modules` folder.**

Existing OCaml web/UI setups usually make you choose between two kinds of pain:

1. Set up a whole cross-compiler JS bundler pipeline that breaks every time someone sneezes on `package.json`.
2. Write raw string-concatenated HTML templates like it's 1999.

**`Quartz`** takes a different route: it gives you **React/Svelte-style reactive signals (`use_state`, `use_signal`, `use_memo`, `use_effect`, `use_history`)**, **Tailwind-style type-safe styling with 60fps fluid cursor spotlights & spring motion**, a **Next.js/Nuxt-style multi-page Router & REST API server**, **pure-SVG data visualizations**, and a **built-in Vite-style build & diagnostic CLI (`quartz check`, `quartz build`, `quartz dev`, `quartz tui`)** that compiles your view tree into:

* **An interactive Browser LiveView + REST API server** (`Quartz.run_browser`) running straight on OCaml `Unix` sockets.
* **A multi-page static HTML/CSS bundle** (`Quartz.run_vite_build`) ready for GitHub Pages / Cloudflare / Vercel.
* **A terminal Flexbox UI** (`Quartz.run_terminal`) for instant CLI dashboards.

---

## Quick Honesty Note

Just keeping it 100% real: I designed `Quartz` because I wanted building websites and dashboards in OCaml to feel as effortless as TypeScript + React/Svelte, and **I used AI to help validate the edge cases and layout math** (like making sure exceptions inside a signal computation don't permanently brick the observer stack). Every module is tested end-to-end with `dune runtest`. And honestly waaaay cooler to do this.

---

## Look How Easy It Is

Open `Quartz` once and you get hooks (`use_state`, `use_signal`, `use_memo`, `use_history`), HTML tags (`div`, `h1`, `p`, `button`, `input`), and pre-styled UI components (`btn`, `spotlight_card`, `stat_card`, `modal`, `drawer`, `area_chart`, `donut_ring`) right at the top level:

```ocaml
open Quartz

let counter_app () =
  (* 1. State & derived values — just like React/SolidJS/Svelte *)
  let count, set_count = use_state 0 in
  let doubled = use_memo (fun () -> !~count * 2) in

  (* 2. Declarative HTML + Tailwind-style OCaml rules + 60fps cursor spotlight *)
  spotlight_card
    ~title:"Quartz Reactive Counter"
    ~subtitle:"Move your cursor over this card for 60fps radial spotlight tracking"
    [
      rx (fun () ->
        p ~style:Style.[ text_lg; color Palette.slate_200 ] [
          textf "Count: %d (Doubled: %d)" !~count !~doubled
        ]);
      div ~style:Style.[ flex_row; gap 10 ] [
        btn ~variant:Primary ~on_click:(fun () -> set_count (!~count + 1)) "+ Increment";
        btn ~variant:Neutral ~on_click:(fun () -> set_count (!~count - 1)) "- Decrement";
      ]
    ]

(* 3. Mount it in a live browser, export static HTML, or print in the terminal! *)
let () =
  Quartz.run_browser ~port:8080 counter_app
```

---

## What Gives Quartz an Edge Over Next.js / Nuxt / Svelte

* **Fine-Grained Signals + Time-Travel History (`Quartz.Signal`):**
  `use_state`, `use_signal`, `use_memo`, `use_effect`, `batch`, `use_store` (Redux/Elm reducer store), `use_history` (`undo` / `redo` time-travel stack), plus `!~` (read signal) and `<~` (write signal) operators.
* **Type-Safe Tailwind Styling & 60fps Fluid Motion (`Quartz.Style` & `Palette`):**
  `flex_row`, `flex_col`, `grid_cols`, `gap`, `p`, `px`, `py`, `rounded_xl`, `glass` (glassmorphism), `hover_lift` (spring physics), `animate_fade_in`, `animate_slide_up`, plus scoped **`hover [...]`**, **`focus [...]`**, **`dark [...]`**, and responsive **`sm` / `md` / `lg` / `xl`** media queries.
* **Next.js / Nuxt-Class Multi-Page Router (`Quartz.Router`):**
  * Dynamic `:param` extraction (`/service/:id`) and wildcard `*` routes
  * Shared nested layouts (`Router.with_layout`) and route guards (`?guard`)
  * Reactive URL query parameters (`query_param`, `set_query`) and automatic `breadcrumbs ()`
  * Compile-time dead-link & route graph verification (`Router.audit_routes`)
* **Embedded JSON REST APIs (`Quartz.Server`):**
  Register `api_get "/api/metrics"` and `api_post "/api/deploy"` right alongside your UI routes in the same ~4MB native OCaml binary.
* **Zero-JS Pure SVG Charting Engine (`Quartz.Charts`):**
  `area_chart` (with SVG `<linearGradient>` underfill & grid lines), `donut_ring` (circumference `stroke-dasharray` gauges), `multi_line_chart` (multi-series with legend), `heatmap` (intensity matrix), `sparkline`, and `bar_chart`.
* **Shadcn-Inspired UI Kit (`Quartz.Components`):**
  `spotlight_card`, `navbar`, `tabs`, `card`, `stat_card`, `badge`, `btn`, `alert`, `modal`, `drawer`, `accordion`, `switch_toggle`, `slider`, `progress_bar`, `toast_rack` (`push_toast`), `code_block`, and `data_table`.
* **Built-In Vite-Style Doctor & Bundler (`quartz check` & `quartz build`):**
  Audits your entire route table, verifies every internal `<a href>` link against registered route patterns, checks DOM accessibility, inspects registered REST APIs, probes TCP port readiness, and builds your static site in under 5 milliseconds.

---

## Build & Run

```bash
git clone https://github.com/cryogenicdeadfrost/quartz.git
cd quartz

# Build library & CLI
dune build

# Run the unit & integration test suite
dune runtest

# Run the Vite-like Doctor & Static Analyzer (`quartz check`)
dune exec quartz -- check

# Build the multi-page production static bundle to ./dist_site (`quartz build`)
dune exec quartz -- build

# Start the interactive LiveView + REST API server on http://127.0.0.1:8080 (`quartz dev`)
dune exec quartz -- dev 8080

# Render the component tree in the Flexbox Terminal UI (`quartz tui`)
dune exec quartz -- tui
```

---

## License

MIT cause ain't noone paying for ts.
