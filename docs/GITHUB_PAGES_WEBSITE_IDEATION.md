# Ideation: Hosting Interactive OCaml Websites on GitHub Pages

GitHub Pages (`https://<username>.github.io/<repo>/`) only serves static files—there is no persistent UNIX socket or Node.js process running on GitHub's origin servers. Despite that constraint, an OCaml application built with **Quartz** can deliver a fast, rich, interactive multi-page experience on GitHub Pages by combining **Ahead-of-Time (AOT) Static HTML Pre-rendering**, **Islands Hydration**, **404.html SPA Route Recovery**, and **Browser-Native Compute (WebGL2/WebGPU + ESM Plugs)**.

---

## 1. How Quartz Bridges OCaml to GitHub Pages

When you run `dune exec quartz -- build` (or `dune exec examples/starter_website.exe -- export dist_site`), Quartz produces a self-contained static directory (`dist_site/`):

1. **Zero-Latency Initial Paint (Pre-rendered HTML + Atomic CSS):**
   Every route (`/`, `/docs`, `/releases`, `/playground`) is executed in native OCaml at build time and emitted as static HTML with deduplicated, content-hashed CSS (`assets/styles.<hash>.css`).
2. **Deep-Link & Refresh Support (`404.html` + `.nojekyll`):**
   GitHub Pages serves `404.html` with HTTP 404 when a user refreshes a dynamic client path, and by default Jekyll ignores files starting with `_`. Quartz automatically writes `.nojekyll` and generates a `404.html` entry point so deep links and client-side transitions work without server rewrites.
3. **Selective Client Interactivity (`assets/quartz-runtime.<hash>.js`):**
   - **3D Canvases (`[data-q-scene3d]`):** Hydrate directly into 60fps WebGPU or WebGL2 scenes in the visitor's browser.
   - **Headless WAI-ARIA Primitives:** Dialogs, tabs, accordions, comboboxes, and roving tabindex menus run client-side keyboard state machines.
   - **Universal NPM Client Plugs (`[data-q-plug]`):** Dynamically import client-side ESM libraries (such as `monaco-editor`, `chart.js`, `katex`, or `@sqlite.org/sqlite-wasm`) directly from `https://esm.sh` with zero local `node_modules`.

---

## 2. Five High-Impact OCaml Website Concepts for GitHub Pages

### Concept A: **TypeLab — Interactive Hindley-Milner Type Inference & AST Explorer** (Flagship Recommendation)
- **What it is:** An educational interactive web application where engineers type or select ML/OCaml expressions and watch Algorithm W (unification, substitution environments, let-polymorphism, and GADT exhaustiveness checking) step through the syntax tree visually.
- **Why OCaml shines here:** Writing parsers, AST transformations, and unification algorithms in OCaml is vastly cleaner than in JavaScript.
- **How it works on GitHub Pages:**
  - Pre-renders 25 curated interactive lessons as static HTML pages (`/lessons/let-polymorphism`, `/lessons/gadts`, `/lessons/functors`).
  - Uses `Quartz.Charts` (pure SVG) to draw live unification DAGs and type variable substitution steps.
  - Shares custom user snippets via URL query parameters (`?expr=...` parsed by `Router.query_param`).

### Concept B: **ConsensusViz — Distributed Systems & Storage Engine Atlas**
- **What it is:** A visual systems engineering handbook demonstrating how Write-Ahead Logs (WAL), B+Trees, LSM-Tree compactions, and Raft leader election work frame-by-frame.
- **Why OCaml shines here:** Quartz's `Store` and `Sql.Embedded` modules already implement a real WAL and relational engine in pure OCaml.
- **How it works on GitHub Pages:**
  - Each chapter pairs crisp systems prose with an interactive step-clock controller (`Signal.t` tick counter) and a `Charts.heatmap` / `Charts.multi_line_chart` showing disk page layouts and replication lag.

### Concept C: **PacketForge — Interactive RFC Protocol & Cryptography Inspector**
- **What it is:** A live bit-level decoder and visualizer for network and security protocols: RFC 6455 WebSocket frames, FIPS 180-4 SHA-256 message schedules, PBKDF2-HMAC-SHA256 key derivation, and Redis RESP2 wire frames.
- **Why OCaml shines here:** Reuses `Quartz.Crypto_util`, `Quartz.Websocket`, and `Quartz.Db_driver` directly—the exact production algorithms power the visual educational breakdown.
- **How it works on GitHub Pages:**
  - Zero backend required; hex/base64 payloads are decoded into colored byte-offset tables with WCAG AAA contrast-checked badges.

### Concept D: **ShaderDeck — Declarative 3D WebGPU / WebGL2 Portfolio & Benchmark**
- **What it is:** A creative engineering portfolio showcasing procedural 3D geometries (Torus knots, spheres, custom GLSL/WGSL fragment shaders) and RK4 spring physics trajectories (`Style.solve_spring_frames`).
- **How it works on GitHub Pages:**
  - `Scene3d` serializes the OCaml scene graph into declarative JSON attributes on `<canvas data-q-scene3d="...">`.
  - On page load, `quartz-runtime.js` compiles the WGSL/GLSL shaders on the visitor's GPU at 60fps.

### Concept E: **Quartz Atlas — Open-Source Project Portal & Release Hub (`examples/starter_website.ml`)**
- **What it is:** A complete product landing page, searchable architecture documentation site, SQL-backed release changelog, and interactive throughput/cost sizing calculator.
- **Ready Out of the Box:** Already implemented in [`examples/starter_website.ml`](../examples/starter_website.ml).

---

## 3. Blueprint: Deploying the Starter Website to GitHub Pages

1. **Build the Static Site Bundle Locally:**
   ```bash
   opam exec -- dune exec examples/starter_website.exe -- export dist_site
   ```
   This runs the 6-stage static analyzer (checking links, WCAG 2.1 AA, APCA Lc, and WAI-ARIA compliance) and writes:
   - `dist_site/index.html`
   - `dist_site/docs.html`
   - `dist_site/releases.html`
   - `dist_site/playground.html`
   - `dist_site/404.html` & `dist_site/.nojekyll`
   - `dist_site/sitemap.xml`, `dist_site/robots.txt`, `dist_site/llms.txt`
   - `dist_site/assets/quartz-runtime.<hash>.js` & `dist_site/assets/styles.<hash>.css`

2. **Generate the GitHub Actions Workflow:**
   ```bash
   opam exec -- dune exec quartz -- deploy pages .
   ```
   This creates `.github/workflows/pages.yml`, which automatically builds the OCaml project, runs `dune runtest` and `quartz check`, and publishes `dist_site/` to GitHub Pages on every push to `main`.
