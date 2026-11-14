# Quartz ⚡

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![npm required](https://img.shields.io/badge/node__modules-0%20bytes-brightgreen.svg)](#)

**Write UI in OCaml like it's React + Tailwind, without touching `npm`, `webpack`, or a 900MB `node_modules` folder.**

Existing OCaml web/UI setups usually make you choose between two kinds of pain:
1. Set up a whole cross-compiler JS bundler pipeline that breaks every time someone sneezes on `package.json`.
2. Write raw string-concatenated HTML templates like it's 1999.

**`Quartz`** takes a different route: it gives you **React/SolidJS-style hooks (`use_state`, `use_memo`, `use_effect`)**, **Tailwind-style type-safe styling**, a **Shadcn-style component kit** (cards, modals, tables, tabs, pure-SVG charts), and compiles your view tree into:
* **An interactive Browser LiveView server** (`Quartz.run_browser`) running straight on OCaml `Unix` sockets.
* **A static HTML/CSS website** (`Quartz.export_site`) ready for GitHub Pages / Netlify / Vercel.
* **A terminal Flexbox UI** (`Quartz.run_terminal`) for quick CLI dashboards.

---

## Quick Honesty Note 🧠

Just keeping it 100% real: I designed `Quartz` because I wanted building websites and dashboards in OCaml to feel as effortless as TypeScript + React, and **I used AI to help validate the reactive dependency-tracker edge cases and layout math** (like making sure exceptions inside a signal computation don't permanently brick the observer stack). Every module is tested end-to-end with `dune runtest`.

---

## Look How Easy It Is

Open `Quartz` once and you get hooks (`use_state`, `use_memo`), HTML tags (`div`, `h1`, `p`, `button`, `input`), and pre-styled UI components (`btn`, `card`, `stat_card`, `modal`, `sparkline`) right at the top level:

```ocaml
open Quartz

let counter_app () =
  (* 1. State & derived values — just like React/SolidJS *)
  let count, set_count = use_state 0 in
  let doubled = use_memo (fun () -> !~count * 2) in

  (* 2. Declarative HTML + Tailwind-style OCaml rules *)
  div ~style:Style.[ flex_col; gap 16; p 24; bg Palette.slate_900; rounded_xl ] [
    h1 ~style:Style.[ text_2xl; bold; color Palette.sky_400 ] [
      text "Quartz Reactive Counter"
    ];
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
  (* Or: Quartz.export_page ~path:"index.html" counter_app *)
  (* Or: Quartz.run_terminal counter_app *)
```

---

## Batteries Included Out of the Box

You don't need to build every widget from scratch. `Quartz` ships with:

* **Fine-Grained Signals (`Quartz.Signal`):**
  `use_state`, `use_memo`, `use_effect`, `batch`, `use_store` (Redux/Elm reducer store), plus `!~` (read signal) and `<~` (write signal) operators.
* **Type-Safe Tailwind Styling (`Quartz.Style` & `Palette`):**
  `flex_row`, `flex_col`, `grid_cols`, `gap`, `p`, `px`, `py`, `rounded_xl`, `shadow_lg`, plus scoped **`hover [...]`**, **`focus [...]`**, **`dark [...]`**, and responsive **`sm` / `md` / `lg` / `xl`** media queries that compile into a `<style>` block automatically.
* **SPA & Multi-Page URL Router (`Quartz.Router`):**
  `Router.route "/users/:id/settings" (fun params -> ...)` with automatic parameter extraction and `Router.link ~to_:"/pricing"`.
* **Validated Forms (`Quartz.Form`):**
  Built-in validators (`non_empty`, `email`, `min_len`, `int_range`, `float_pos`) and `Form.view_input` which renders the label, input border state, and error message automatically.
* **Shadcn-Inspired UI Kit (`Quartz.Components`):**
  * `navbar`, `tabs`, `card`, `stat_card`, `badge`, `btn`, `alert`, `modal`, `data_table`
  * **Zero-JS Pure SVG Charts:** `sparkline` and `bar_chart` rendered straight to crisp vector `<svg>`.

---

## Build & Run

```bash
git clone https://github.com/cryogenicdeadfrost/quartz.git
cd quartz

# Build library & CLI
dune build

# Run the test suite
dune runtest

# Export the multi-page SaaS demo site to ./dist_site AND preview in Terminal
dune exec quartz

# Or start the interactive LiveView server on http://localhost:8080
dune exec quartz -- serve 8080

# Run the minimal counter example
dune exec examples/counter_and_form.exe
```

---

## License

MIT — go build cool stuff without fighting webpack for 4 hours.
