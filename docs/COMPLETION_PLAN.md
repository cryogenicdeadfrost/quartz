# Quartz -- Completion Plan

What it takes to turn Quartz from a working prototype into a module you can build,
ship, and run a real startup website on. Split into: stuff we write ourselves,
stuff we borrow (because rolling it ourselves would be dumb or dangerous), the
MirageOS unikernel target, motion/3D, and hosting (Vercel and friends).

Status today (v3.0.0): signals, styling, SVG charts, router, renderers and the
check/build CLI work and are tested. The server is a single-threaded prototype
that shares one app state across every visitor. Everything below builds on that
honest baseline.

---

## 0. Ground rules

- Pure OCaml first. Only `stdlib`, `unix`, `threads` unless the row says otherwise.
- Never write our own crypto. TLS, certificates, password hashing = borrowed.
- Every borrowed thing sits behind a Quartz interface (module type / functor),
  so it can be swapped and the core stays dependency-free.
- Toolchain bump: the WSL switch is OCaml 4.13.1. MirageOS 4 wants >= 4.14 and
  effects/Domains want 5.x. Target **OCaml 5.2+**.

---

## 1. Build from scratch (pure OCaml)

| Area | What | Why it's doable in-house |
| :--- | :--- | :--- |
| Core split | Pull a `quartz.core` library out with **zero** `unix` usage: inject a clock instead of calling `Unix.gettimeofday` (used today in `resource`, `isr`, `middleware`, `pubsub`, `vite`) | Required for MirageOS, js_of_ocaml and Vercel adapters |
| Transport-agnostic server | `Server.handle : request -> response` as a pure function; sockets live in adapters (`quartz.unix`, `quartz.mirage`, `quartz.vercel`) | One app, many hosts |
| Sessions | Per-visitor app instance keyed by a signed session cookie, idle eviction | Fixes the shared-state bug in `run_browser` |
| HTTP/1.1 properly | Keep-alive, chunked encoding, `Content-Length` bodies > 16 KB, multipart uploads, cookies, real client IP (today hardcoded `127.0.0.1`) | Plain parsing work |
| WebSockets | RFC 6455 framing + handshake (SHA-1 + base64, ~100 lines) | Handshake hash is not a security boundary |
| Keyed DOM diff | Diff old/new `Dom.node` trees, send small patch ops instead of full `innerHTML` | Pure tree algorithm |
| Real SSE | Keep the connection open and stream `Pubsub` events (today it dumps history and closes) | Needs the threaded/Domain server |
| Hot reload | Watch files via `Unix.stat` mtimes, rerun `dune build`, restart child process, push `reload` over WS/SSE | Dev-only, simple |
| Embedded DB | Append-only log + in-memory index + snapshot/compaction + crash recovery, behind a `STORAGE` module type | Good enough for small sites; swappable later |
| Asset pipeline | Content-hashed CSS/JS/images (`styles.<hash>.css`), immutable cache headers, CSS dedup of atomic rules | String + hashing work |
| Deploy generators | `quartz deploy pages` (GitHub Actions YAML), `quartz deploy vercel` (`.vercel/output`), `quartz deploy docker` (Dockerfile) | Just file emitters |
| Typed routes | GADT path combinators: wrong param type on a link = compile error | OCaml-only superpower |
| Shared API types | One OCaml type -> server handler + generated client call; no drift | OCaml-only superpower |
| Effects (OCaml 5) | `Suspense` and streaming SSR without async/await colouring | OCaml-only superpower |
| Multicore | Domain pool for request handling / parallel SSR | OCaml 5 stdlib |
| Spring easing | Compute spring curves in OCaml, emit CSS `linear(...)` easing | Math we already like |
| Color science | OKLCH palette generator + APCA contrast next to WCAG 2.1 | Math we already like |

---

## 2. Borrow (third-party, justified)

| Need | Library | Why not from scratch |
| :--- | :--- | :--- |
| TLS / HTTPS | `ocaml-tls`, `mirage-crypto`, `x509` (or terminate at Caddy/nginx/the host) | Crypto. Not touching it. |
| Auto certificates | `letsencrypt` (ocaml-letsencrypt) | ACME + crypto |
| Password hashing | `argon2` / `bcrypt` bindings | Crypto |
| Hashes for protocols | `digestif` (SHA-256/HMAC for Postgres SCRAM, signed cookies) | Correctness > pride |
| Code in the browser | `js_of_ocaml` / `wasm_of_ocaml` | A compiler backend is its own project |
| Browser APIs | `brr` (DOM, Canvas, WebGL2, WebGPU bindings) | Huge surface, well maintained |
| Postgres | `pgx` (pure OCaml client) or `caqti` (Postgres + SQLite) | Wire protocol + SCRAM auth |
| SQLite | `sqlite3-ocaml` (via `caqti`) | C library |
| Git-like versioned store | `irmin` (also runs inside MirageOS) | Great fit for unikernels |
| Async I/O (optional) | `eio` (effects-based, OCaml 5) | Saves us writing an event loop per platform |
| HTTP/2, HTTP/3 | `h2` / `httpun` | Big specs, low payoff to rewrite |
| JSX-like syntax | `ppxlib` | Needed for any ppx |
| Unikernel | `mirage`, `mirage-tcpip`, `paf` / `cohttp-mirage`, `crunch` | See section 3 |

---

## 3. MirageOS unikernel target (`quartz.mirage`)

A unikernel compiles the app, network stack and a tiny runtime into one bootable
image. No Linux, no shell, no package manager.

**What you get:** images of a few MB, millisecond boot, tiny attack surface
(nothing to SSH into, no unused services), and the whole stack is type-checked
OCaml.

**Architecture**

```text
              +-------------------------------+
  Browser --> |  Quartz app (quartz.core)     |   pure: signals, router, dom, styles
              |  Server.handle req -> res     |
              +-------------------------------+
              |  quartz.mirage adapter        |   paf / cohttp-mirage HTTP, sessions
              +-------------------------------+
              |  mirage-tcpip  | ocaml-tls    |   network + HTTPS (+ letsencrypt)
              +-------------------------------+
              |  crunch (static assets)       |   dist_site baked into the image
              |  irmin / mirage-kv (storage)  |
              +-------------------------------+
              |  Solo5 (hvt / virtio) or Xen  |   KVM hosts, cloud VMs
              +-------------------------------+
```

**Sketch of `config.ml`** (exact device signatures depend on the MirageOS version):

```ocaml
open Mirage

let main =
  main "Unikernel.Make"
    ~packages:[ package "quartz"; package "paf" ]
    (stackv4v6 @-> kv_ro @-> job)

let stack = generic_stackv4v6 default_network
let assets = crunch "dist_site"

let () = register "quartz-site" [ main $ stack $ assets ]
```

**Steps**
1. OCaml 5.2+ switch, install `mirage`.
2. Ship the `quartz.core` split from section 1 (the unikernel cannot link `unix`).
3. Write `quartz.mirage`: HTTP via `paf`, clock via Mirage time devices, assets via `crunch`.
4. `mirage configure -t unix` first (runs as a normal process, easy to debug).
5. Then `-t hvt` (KVM) or `-t virtio` (cloud VMs that accept custom images).
6. HTTPS inside the image with `ocaml-tls` + `letsencrypt`, or put a load balancer in front.

**Limits to be honest about:** one CPU per unikernel (scale by running more of
them), no local filesystem beyond KV/block devices, harder debugging, and each
cloud provider needs its own custom-image upload.

---

## 4. Motion, color and live 3D

Animations and 3D run **in the browser**. The server or unikernel only ships the
bytes. LiveView round trips (~20-100 ms) are fine for clicks but useless for
60 fps motion, so Quartz has to generate client-side CSS/JS for anything that moves.

**CSS-only (no JS, generated by `Quartz.Style`):**
- `@keyframes` builder, cross-document View Transitions (`@view-transition`)
- Scroll-driven animations (`animation-timeline: scroll()` / `view()`)
- `@property` typed custom properties (animatable gradients, glows)
- Spring physics via CSS `linear(...)` easing computed in OCaml
- OKLCH palettes, `color-mix()`, container queries
- `prefers-reduced-motion` respected automatically

**Needs a browser runtime (`js_of_ocaml` / `wasm_of_ocaml` + `brr`):**
- Gesture/drag physics, FLIP list reordering, magnetic cursors
- Live 3D: a small OCaml scene graph (meshes, camera, lights) rendered via
  WebGPU with a WebGL2 fallback; or typed bindings to three.js shipped as a
  static asset
- Shaders written as OCaml-generated WGSL/GLSL strings with typed uniforms

**Bottom line:** with a client runtime, Quartz can do anything a React + three.js
site can. Without one, it's capped at what CSS can do (which is a lot in 2026).

---

## 5. Hosting

| Host | Static (`quartz build`) | Live server (SSR / LiveView) |
| :--- | :--- | :--- |
| GitHub Pages | Yes | No (static only) |
| Vercel | Yes (`.vercel/output/static`) | Via an adapter, see below |
| Netlify / Cloudflare Pages | Yes | Workers/Functions via JS/Wasm build |
| Fly.io / Railway / Render / Cloud Run | Yes | **Yes, as-is** in a Docker image |
| VPS / bare metal | Yes | Yes (binary or unikernel) |
| KVM / Xen hosts | -- | MirageOS unikernel |

### Why Vercel doesn't just work today

1. Vercel runs **per-request functions**, not a process that calls `Unix.bind`
   and loops on `accept`. Quartz's server is that kind of loop.
2. OCaml isn't an official Vercel runtime (official: Node.js, Bun, Python, Go, Ruby).
3. Quartz keeps session state in process memory; serverless instances come and
   go, so state has to live in Redis/KV/Postgres or in the browser.
4. Vercel functions now accept WebSockets, but connections are pinned to one
   instance and capped by function timeouts, so LiveView needs reconnect logic
   and external pub/sub.

### `quartz.vercel` adapter plan

- **Route A, native binary:** compile a static Linux binary (musl) that speaks
  the AWS Lambda Runtime API loop (HTTP calls to a local endpoint, doable with
  `unix` alone), name it `bootstrap`, and emit it through Vercel's Build Output
  API as a custom-runtime function. This is how community runtimes such as
  vercel-rust work. Verify the current `.vc-config.json` fields against Vercel's
  docs before relying on them.
- **Route B, JS build:** compile `Server.handle` with `js_of_ocaml` and wrap it
  as a Node.js function. Officially supported runtime, gets Vercel's WebSocket
  support for free. Slower than native, zero platform risk.
- Both need: the transport-agnostic `Server.handle`, sessions in an external
  store, and `quartz deploy vercel` to generate `.vercel/output`.

---

## 6. Phased roadmap

| Version | Scope |
| :--- | :--- |
| v3.1 | Core split (no `unix`), clock injection, `Server.handle`, sessions, proper HTTP/1.1, real client IP |
| v3.2 | WebSockets, keyed DOM diff, streaming SSE, hot reload |
| v3.3 | Embedded log store + `STORAGE` functor; `pgx`/`caqti`/`irmin` adapters |
| v3.4 | Browser runtime via `js_of_ocaml`/`wasm_of_ocaml` + `brr`; hydration |
| v3.5 | Asset hashing, `quartz deploy pages / vercel / docker`, GitHub Actions CI |
| v4.0 | OCaml 5: Domain pool, effects-based Suspense + streaming SSR, typed GADT routes, shared API types |
| v4.1 | `quartz.mirage` unikernel target (unix -> hvt -> virtio), TLS + letsencrypt |
| v4.2 | Motion engine (keyframes, view/scroll transitions, springs, OKLCH/APCA) + WebGPU/WebGL2 3D scene graph |

---

## 7. Bare minimum to host a real website in 2026

Two tiers. A **static site** (portfolio, landing page, docs) needs rows 1-10.
A **dynamic site** (accounts, forms, dashboards) needs all of them.

Status key: **Have** = works in v3.0.0, **Partial** = exists but incomplete, **Missing** = not built.

| # | Requirement | Status | How Quartz gets it |
| :--- | :--- | :--- | :--- |
| 1 | Domain + DNS | n/a | Registrar + host DNS. Nothing for Quartz to do. |
| 2 | HTTPS | Missing | Host terminates TLS (Vercel, Pages, Fly, Cloud Run), or Caddy in front. Unikernel: `ocaml-tls` + `letsencrypt`. |
| 3 | Correct HTTP | Partial | Real 404/500 status codes (router 404 currently returns 200), 301 redirects, `HEAD`, `ETag` / `Cache-Control`. From scratch. Compression via `decompress` (pure OCaml) or let the host/CDN compress. |
| 4 | Static files | Missing | Serve `/public` (images, fonts, favicon, `manifest.webmanifest`) with correct MIME types; copy into `dist_site` on build; hashed filenames. From scratch. |
| 5 | SEO + social cards | Partial | Have: `<title>`, `sitemap.xml`, `robots.txt`. Need: per-route `meta description`, canonical URL, Open Graph / Twitter tags, JSON-LD. Add a `?meta` record to `Router.route`. From scratch. |
| 6 | Responsive + accessible | Partial | Have: viewport tag, breakpoints, WCAG contrast audit, `alt` check. Need: visible focus styles, keyboard nav for modal/drawer/palette, `aria-*` on interactive components, `lang` per page. From scratch. |
| 7 | Performance (Core Web Vitals) | Partial | Have: tiny HTML/CSS, zero JS. Need: `width`/`height` + `loading="lazy"` on images, `font-display: swap`, preload hints. Image resizing/WebP/AVIF at build time: shell out to `cwebp` / `libvips` (borrowed tools). |
| 8 | Works without the live server | Missing | Static export has dead buttons today. Either progressive enhancement (plain `<form method="post">`, `<details>` instead of JS accordions) or the browser runtime (v3.4). |
| 9 | Security basics | Partial | Have: security headers, CSRF shield, HTML escaping. Need: Content-Security-Policy, secrets from env vars, request size limits. From scratch. |
| 10 | Build + deploy pipeline | Missing | `quartz deploy pages / vercel / docker` + GitHub Actions running `dune build`, `dune runtest`, `quartz check` on every push. From scratch (YAML/file emitters). |
| 11 | Per-visitor sessions | Missing | v3.1 (cookie-keyed sessions). Serverless hosts: store in Redis/KV/Postgres. |
| 12 | Persistent data + backups | Missing | v3.3 embedded store, or `pgx` / `caqti` to a managed Postgres. Backups = host feature or scheduled dump. |
| 13 | Auth | Missing | Password hashing borrowed (`argon2`); sessions + login flows from scratch. Or OAuth via a provider's HTTP API. |
| 14 | Forms + email | Partial | Have: validators. Need: server form endpoint, honeypot + rate limit for spam, email sending via a provider HTTP API (Resend, Postmark) or SMTP (borrowed client). |
| 15 | Observability | Partial | Have: `/api/health`, request timer header. Need: structured access logs, error capture, uptime pings (external service). |
| 16 | Privacy / legal | n/a | Privacy policy page; cookie banner only if you add tracking. Server-side page counts avoid needing one. |

**Shortest path to a hostable static site:** rows 3, 4, 5, 8 (progressive enhancement) and 10. All from scratch, no new dependencies.

**Shortest path to a hostable dynamic site:** the above + v3.1, v3.3 and rows 13-15, deployed as Docker on Fly / Railway / Cloud Run.

---

## 8. One core, many targets

There is no single binary that runs everywhere, but there can be a single *codebase*:

- The app is written once against `Server.handle : request -> response` (the same
  shape as the Fetch API `Request -> Response` that Node, Bun, Deno, Vercel and
  Cloudflare all use).
- Two build outputs cover nearly every host:
  1. **Native static binary (musl, x86_64 + arm64):** Docker (Fly, Railway, Render,
     Cloud Run), VPS, AWS Lambda / Vercel custom runtime (`bootstrap`), MirageOS.
  2. **JS / Wasm module (`js_of_ocaml` / `wasm_of_ocaml`) exporting `fetch(request)`:**
     Vercel Node + Edge, Cloudflare Workers, Netlify, Deno Deploy, Bun.
- Thin adapters translate each host's request format into `Server.handle`:
  plain socket loop, Lambda Runtime API loop, Mirage `paf`, JS `fetch`.
- `quartz deploy <target>` picks the build output and adapter.

Why native alone is not one-size-fits-all: edge platforms (Cloudflare Workers,
Vercel Edge) only run JS/Wasm, every host packages binaries differently, and
CPU architecture differs (x86_64 vs arm64). Why JS/Wasm alone isn't either: no
`unix`, no threads/Domains, slower, and no unikernel.
