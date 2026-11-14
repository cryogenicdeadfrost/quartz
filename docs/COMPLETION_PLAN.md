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

## 8. One Core, Many Targets (Detailed Architecture & Grounded Blueprint)

There is no single binary format that executes on bare-metal KVM, Linux Docker,
Vercel Edge V8 isolates, and a visitor's browser GPU at the same time. Instead,
Quartz achieves **one codebase for every platform** by separating **pure application
logic (`quartz.core`)** from **host runtime adapters** and **client browser islands**.

### 8.1 The Strict Boundary Rule (How Unikernels & JS Coexist)

```text
+-------------------------------------------------------------------------+
|                       YOUR APPLICATION CODEBASE                         |
|   Routes, Signals, Components, WCAG/OKLCH Styles, Server Actions, APIs  |
|                (Written once in 100% pure, type-safe OCaml)             |
+-------------------------------------------------------------------------+
                                    |
                     Compiles against `quartz.core`
              (ZERO `Unix`, ZERO `Js_of_ocaml` in core types)
                                    |
        +---------------------------+---------------------------+
        |                                                       |
        v (Server / Edge / Unikernel Targets)                   v (Browser Target)
+-----------------------------------------------+   +-----------------------------+
|          `Server.Make (Host : HOST)`          |   |      `quartz.browser`       |
|  Pure `handle : request -> response` functor  |   | (`js_of_ocaml` / `wasm` +   |
+-----------------------------------------------+   |  `brr` WebGPU/WebGL2/JS FFI)|
   |              |                |           |    +-----------------------------+
   v              v                v           v                   |
+--------+  +-----------+  +-------------+  +------+               |
| Mirage |  |   Unix    |  |   Vercel    |  | Edge |               |
|Unikern.|  | Multicore |  |  Bootstrap  |  |Worker|               |
|(5-8 MB)|  |(Docker/VM)|  |(Lambda API) |  |(JS)  |               |
+--------+  +-----------+  +-------------+  +------+               |
   |              |                |           |                   |
   +--------------+----------------+-----------+-------------------+
                                   |
                    Ships static `.js` / `.wasm` asset
                    to the browser for 60fps 3D & Islands
```

Why this never breaks the Unikernel or OCaml's speed:
1. **`quartz.core` is 100% OS-independent:** It never calls `Unix.gettimeofday`,
   `Unix.socket`, or browser globals directly. Time, storage, sessions, and pub/sub
   are passed in via an OCaml **Functor (`Server.Make`)**.
2. **The Unikernel never runs JavaScript:** When you use 3D WebGL/WebGPU, complex
   client gestures, or a third-party JS library (like Three.js or Stripe), that code
   is compiled separately by `js_of_ocaml` / `wasm_of_ocaml` into a static
   `app.client.js` asset. `crunch` bakes that static file into the MirageOS image,
   and the Unikernel simply streams those bytes over TCP/TLS at bare-metal speed.

### 8.2 Exact OCaml Functor Signatures (`quartz.core`)

```ocaml
module type CLOCK = sig
  val now_s : unit -> float
  val now_us : unit -> int64
end

module type STORAGE = sig
  type 'a io
  val get : string -> string option io
  val set : ?ttl_s:int -> string -> string -> unit io
  val delete : string -> unit io
  val append_log : topic:string -> string -> unit io
end

module type PUBSUB = sig
  val publish : topic:string -> string -> unit
  val subscribe : topic:string -> (string -> unit) -> int
  val unsubscribe : int -> unit
end

(* Parameterizing the app over CLOCK, STORAGE, and PUBSUB lets the exact same
   route table, middleware chain, and SSR engine run on Mirage, Unix, or Vercel *)
module Make_app (C : CLOCK) (S : STORAGE) (P : PUBSUB) = struct
  val handle : Request.t -> Response.t S.io
end
```

### 8.3 The 5 Concrete Compilation Targets

| Target Adapter | Compiler Backend | How Requests Arrive & State Lives | Where It Deploys |
| :--- | :--- | :--- | :--- |
| **`quartz.mirage`** | `ocamlopt` + Solo5 / Xen | `paf` + `mirage-tcpip` + `ocaml-tls`; static assets baked via `crunch`; storage via `irmin` / block KV | Bare-metal KVM, Ukvm, AWS EC2/Firecracker, Vultr, Equinix |
| **`quartz.unix`** | `ocamlopt` (OCaml 5 Domains) | Multi-core socket pool + keep-alive + native WebSockets; dev mode adds `Unix.stat` Hot-Reload | Docker on Fly.io, Railway, Render, Cloud Run, DigitalOcean, VPS |
| **`quartz.vercel_native`** | `ocamlopt -ccopt -static` (musl Linux) | Speaks the AWS Lambda Runtime HTTP API (`GET /runtime/invocation/next` -> `Server.handle` -> `POST /response`) as a `bootstrap` binary inside `.vercel/output/functions/*.func/` | Vercel Functions (`provided.al2023`), AWS Lambda, Netlify Functions |
| **`quartz.edge_js`** | `js_of_ocaml` or `wasm_of_ocaml` | Exports standard WinterCG `export default { async fetch(req, env) }` wrapping `Server.handle`; state in Vercel KV / Upstash Redis / Cloudflare D1 | Vercel Node & Edge, Cloudflare Workers, Deno Deploy, Bun |
| **`quartz.browser`** | `js_of_ocaml` / `wasm_of_ocaml` + `brr` | Hydrates `[data-q-island]` nodes in the DOM, runs client Signals offline, drives WebGPU/WebGL2 3D canvases, and calls arbitrary npm/JS packages via typed FFI | Runs inside every visitor's browser |

---

## 9. Beyond the Bare Minimum: Porting the Full JS Powerhouse Ecosystem
**(Bun + Nuxt + Next.js + NestJS + Express + SvelteKit + Framer Motion + Three.js)**

To beat modern JS stacks rather than merely catching up to them, here is how every
advanced capability from the 2026 JS ecosystem maps into Quartz -- and what we build
vs. bridge.

### 9.1 Backend, API, Middleware, Cookies, Sessions & Database (NestJS / Express / Nuxt Nitro / Bun)

| Capability | JS Inspiration | Quartz Implementation (Pure OCaml + Plugged Drivers) |
| :--- | :--- | :--- |
| **Signed & Encrypted Cookies + Session Store** | Express `cookie-session` / NextAuth / Nuxt `useSession` | **From scratch + `digestif` HMAC-SHA256:** `Cookie.parse`, `Cookie.set_signed ~http_only:true ~same_site:`Lax ~secure:true`, automatic per-visitor session table with pluggable `Memory`, `File`, `Redis`, or `Irmin` backing. |
| **Compile-Time Dependency Injection & Modules** | NestJS `@Module()`, `@Injectable()`, `CanActivate` Guards, Interceptors | **From scratch (OCaml Functors):** Instead of NestJS runtime reflection (`reflect-metadata`), services are OCaml module signatures (`AUTH_SERVICE`, `MAILER`, `DB`). Guards and Interceptors are typed higher-order middlewares (`Middleware.guard`, `Middleware.intercept`). |
| **End-to-End Typed RPC & Server Functions** | Next.js `"use server"` / Nuxt `$fetch` / tRPC | **From scratch:** Shared OCaml request/response types (`'req, 'res) Endpoint.t`) compiled simultaneously into the server route handler and the client `fetch` caller. Changing a field on the server breaks the client build immediately. |
| **Database Layer, Migrations & Query Safety** | Prisma / Drizzle ORM / Bun `bun:sqlite` | **Hybrid:** Built-in zero-dependency append-only log + B-tree index store (`Quartz.Store`) for instant single-binary apps; functor adapters for **`caqti` (PostgreSQL / SQLite)**, **`pgx` (pure-OCaml Postgres)**, and **`irmin` (Git-like branching DB for Unikernels)** with schema migration runner (`quartz db migrate`). |
| **Multipart Uploads, Streaming & Range Requests** | Express `multer` / Bun `Bun.file` | **From scratch:** Streaming `multipart/form-data` parser with byte-size limits, MIME magic-byte sniffing, and `Accept-Ranges` / `206 Partial Content` for video/audio streaming. |
| **Background Jobs, Cron & Queues** | BullMQ / Nuxt Nitro Tasks / Vercel Cron | **From scratch:** In-process fiber/Domain task queue (`Jobs.schedule ~every:60.0`, `Jobs.enqueue ~retries:3`) plus HTTP webhook trigger endpoints (`/api/__cron/*`) for serverless hosts. |
| **Real-Time WebSockets + PubSub Rooms** | `Bun.serve` WebSockets / Socket.io / PartyKit | **From scratch:** RFC 6455 WebSocket upgrade + binary/text framing + topic rooms (`Ws.join "room:42"`, `Ws.broadcast "room:42" msg`) so multiplayer cursors, chat, and live order books stream at <1 ms latency. |

### 9.2 Frontend, Routing, Islands, Motion, Color Science & Live 3D (Next / SvelteKit / Framer / Three.js)

| Capability | JS Inspiration | Quartz Implementation (Pure OCaml + `quartz.browser`) |
| :--- | :--- | :--- |
| **Partial Hydration ("Islands Architecture")** | Astro Islands / Fresh / Next Server + Client Components | **From scratch + `js_of_ocaml`:** Static text/layout ships as **0 KB JS** HTML from the server/Unikernel. Interactive widgets marked `Dom.island ~name:"chart" ~props view` hydrate independently in the browser via `quartz.browser` -- so tabs, modals, and sliders work offline and on static hosts! |
| **GADT Type-Safe Router + Parallel/Modal Routes** | Next.js App Router / TanStack Router / SvelteKit | **From scratch:** GADT path combinators (`Path.(s "org" / str / s "item" / int)`) + loader functions (`~loader`) + optimistic prefetching on hover + browser History API + modal/intercepting routes. |
| **OKLCH Perceptual Color Engine & APCA Contrast** | Culori / Tailwind v4 OKLCH / Radix Colors | **From scratch (`lib/style/contrast.ml`):** Full CIE XYZ <-> OKLab <-> OKLCH color-space math in pure OCaml. Generate entire 11-step accessible color scales from a single brand hex code (`Palette.generate_scale`), and audit both WCAG 2.1 and **APCA (Accessible Perceptual Contrast Algorithm)** during `quartz check`. |
| **Physics-Based Motion & FLIP Layout Transitions** | Framer Motion (`layoutId`) / Svelte `transition:` / GSAP | **From scratch + CSS/WAAPI:** Solve RK4/verlet spring differential equations in OCaml to emit exact CSS `linear(...)` easing curves, `@starting-style` enter animations, cross-route `@view-transition`, scroll-timeline bindings, and FLIP bounding-box morphing in `quartz.browser`. |
| **Live 3D Scene Graph (WebGPU / WebGL2 / Three.js)** | React Three Fiber (`@react-three/fiber`) / Three.js | **Declarative OCaml 3D AST (`Quartz.Scene3d`) + `brr`:** Write declarative 3D scenes in OCaml (`Scene3d.canvas [ Scene3d.perspective_camera ~fov:45. (); Scene3d.point_light (); Scene3d.mesh ~geometry:Box ~material:(Pbr { metalness = 0.8; roughness = 0.2 }) ~rotate_sig () ]`). Compiles to a client WebGL2/WebGPU loop or binds directly to `Three.js` via `js_of_ocaml` FFI, reactive to the exact same `Signal.t` values as your 2D UI! |
| **Zero-Cost JS Ecosystem FFI Bridge (`Quartz.Js_bridge`)** | Any npm package (Stripe, Mapbox, Monaco Editor, Chart plugins) | **Via `js_of_ocaml` `Jv`:** A tiny typed wrapper (`Js_bridge.mount_external ~global:"Stripe" ~props`) lets a Quartz island instantiate and control any browser JS library while keeping state transitions typed in OCaml. |

### 9.3 Dev Tooling, Sub-200ms Hot Reload & One-Command Deploys (Vite / Bun)

| Capability | JS Inspiration | Quartz Implementation (`lib/tooling/vite.ml`) |
| :--- | :--- | :--- |
| **Instant Dev Hot-Reload (`quartz dev`)** | Vite HMR / `bun --hot` | Polls file mtimes (`Unix.stat`) every 80 ms -> runs incremental `dune build` (<200 ms) -> restarts server worker -> pushes a `{"type":"hmr_reload"}` frame over `/__quartz_ws` so the browser updates automatically without losing scroll position or form inputs. |
| **Asset Fingerprinting, CSS Dedup & Bundles** | Vite Rollup / Turbopack | Deduplicates all atomic CSS rules across routes, emits content-hashed `/assets/styles.<sha1>.css` and `/assets/client.<sha1>.js` with `Cache-Control: public, max-age=31536000, immutable`, and copies `/public/*`. |
| **One-Command Multi-Cloud Deploy (`quartz deploy`)** | `vercel deploy` / `fly launch` / `wrangler deploy` | `quartz deploy vercel` (emits `.vercel/output/config.json`, static assets, and `bootstrap` / Edge functions), `quartz deploy fly` (emits multi-stage Alpine/musl `Dockerfile` + `fly.toml`), `quartz deploy pages` (emits `.github/workflows/pages.yml`), and `quartz deploy mirage` (emits `config.ml` + `unikernel.ml` for MirageOS). |

---

## 10. Grounded Complexity & Line-of-Code Budget

Everything above is modular, incremental, and does not turn Quartz into a bloated
monolith:

| Subsystem Package | Target Environment | Est. Size (LOC) | External Dependencies |
| :--- | :--- | :--- | :--- |
| **`quartz.core`** (Signals, Resource, Style, OKLCH/APCA, DOM diff, GADT Router, Form, Charts, Components, Scene3d AST, Server.Make) | Everywhere (Native, Unikernel, JS, Wasm) | ~3,800 LOC | **None** (`stdlib` only) |
| **`quartz.unix`** (HTTP/1.1 keep-alive, WebSockets, Multipart, Cookie Sessions, Embedded Store, HMR Watcher, Deploy CLI) | Local Dev, Docker, Fly, Railway, VPS | ~1,400 LOC | `unix`, `threads` (OCaml stdlib) |
| **`quartz.vercel`** (AWS Lambda Runtime loop `bootstrap` + `.vercel/output` builder) | Vercel Functions, AWS Lambda | ~350 LOC | `unix` (or `js_of_ocaml` for Edge) |
| **`quartz.mirage`** (Unikernel adapter over `paf`, `mirage-tcpip`, `crunch`, `irmin`) | Solo5 KVM (`hvt`), `virtio`, Xen | ~400 LOC | `mirage`, `paf`, `ocaml-tls`, `irmin` (optional package) |
| **`quartz.browser`** (Client Island Hydrator, Offline Signals, FLIP Motion, WebGL2/Three.js 3D runner, JS FFI) | Browser (`app.client.js` / `.wasm`) | ~1,100 LOC | `js_of_ocaml`, `brr` (optional package) |

**Total Core + All Adapters:** ~7,000 lines of clean, human-readable OCaml -- smaller
than a single sub-package of Next.js or Webpack, compiling in milliseconds, and
running everywhere from a 5 MB bare-metal Unikernel to Vercel and the browser GPU.

