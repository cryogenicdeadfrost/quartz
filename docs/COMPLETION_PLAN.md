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
| **`quartz.vercel_native`** | `ocamlopt -ccopt -static` (musl Linux) | Speaks the AWS Lambda Runtime HTTP API (`GET /runtime/invocation/next` -> `Server.handle` -> `POST /response`) as a `bootstrap` binary inside `.vercel/output/functions/*.func/` | AWS Lambda (`provided.al2023`, officially supported). Vercel custom runtimes are community-maintained (same pattern as `vercel-rust`) -- must be verified per release; `quartz.edge_js` is the guaranteed Vercel path. |
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
| **Real-Time WebSockets + PubSub Rooms** | `Bun.serve` WebSockets / Socket.io / PartyKit | **From scratch:** RFC 6455 WebSocket upgrade + binary/text framing + topic rooms (`Ws.join "room:42"`, `Ws.broadcast "room:42" msg`) so multiplayer cursors, chat, and live order books stream with low latency (in-process fan-out is microseconds; real-world latency is dominated by network RTT, so no "<1 ms" end-to-end promise). |

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

**Total Core + All Adapters (original estimate):** ~7,000 LOC. See 10.1 for the
corrected, realistic estimate.

### 10.1 Corrected (Realistic) Budget

The table above underestimates the work. Production-grade WebSockets, multipart,
sessions, a GADT router, OKLCH/APCA, Scene3d, HMR, accessible headless primitives,
and four deploy targets -- each with tests -- add up to roughly 2-3x more:

| Package | Realistic LOC (incl. tests) | Notes |
| :--- | :--- | :--- |
| `quartz.core` | 8,000 - 11,000 | Router, signals, DOM diff, style, color, forms, charts, a11y primitives |
| `quartz.unix` | 3,000 - 4,500 | HTTP/1.1 parser hardening, WebSockets, multipart, sessions, store, HMR |
| `quartz.edge_js` + `quartz.vercel_native` | 1,200 - 2,000 | Promise bridge, Fetch request/response mapping, Lambda loop, output builder |
| `quartz.mirage` | 800 - 1,500 | Functor glue, `crunch` assets, KV storage, TLS config |
| `quartz.browser` | 2,500 - 4,000 | Hydrator, shared runtime chunk, FLIP motion, 3D runner, FFI bindings |
| CLI + deploy + docs examples | 1,500 - 2,500 | `quartz new/dev/check/build/deploy`, templates |

**Realistic total: ~17,000 - 25,000 LOC.** That is still small next to Next.js
(hundreds of thousands of LOC) and realistic for 1-2 maintainers over 9-12 months.
"Compiles in milliseconds" applies to incremental rebuilds of one module; a clean
build of everything takes seconds.

### 10.2 Corrections to Earlier Claims

| Earlier Claim | Correction |
| :--- | :--- |
| `quartz.core` has zero `Unix` | **Not yet true.** `lib/backend/server.ml` and `lib/tooling/vite.ml` call `Unix` directly and the `quartz` library depends on `unix`. Step 1 of v4 is the physical split into `quartz.core` (no `unix`) + `quartz.unix`. |
| Hello-world js_of_ocaml bundle is ~25-40 KB | Depends heavily on what is linked. Pure DOM code dead-code-eliminates well; using `Printf`/`Format` adds a lot of weight. `wasm_of_ocaml` requires WasmGC and output is larger. Budget is enforced by measurement, not assumed. |
| Each island hydrates independently | All islands on a page must share **one** runtime chunk (`quartz-runtime.<hash>.js`) plus small per-island entry modules, or each island pays the runtime cost again. |
| DevTools formatter shows OCaml variant names | Compiled OCaml keeps only numeric tags at runtime. Names are recoverable only if dev builds emit a debug name table (ppx-generated `[@@deriving quartz_debug]`). Source maps (`--source-map`) work today. |
| HTML -> `Quartz.Dom` codegen gives shadcn/Radix parity | Codegen copies **markup only**. Most of Radix's value is behaviour (focus trap, roving tabindex, ARIA state, dismiss layers, portals). Parity needs a native headless primitives library (see 11.5). |
| `quartz.edge_js` lets V8 run the server "natively" | It runs as ordinary JS/Wasm in V8. Cold start scales with bundle size, and Workers enforce compressed script size limits, so the edge build must be tree-shaken and measured in CI. |

---

## 11. Known Gaps vs. JavaScript and Concrete Solutions

| # | Gap | Why JS Wins | Solution for Quartz | Status Target |
| :--- | :--- | :--- | :--- | :--- |
| 11.1 | **Async model fragmentation** | JS has one universal `Promise`. OCaml has Lwt, Eio, and Async; OCaml 5 effects in js_of_ocaml require CPS translation with a performance cost. | `quartz.core` defines one minimal `IO` signature (`return`, `bind`, `map`). Adapters: Lwt for `quartz.mirage` (Mirage is Lwt-native), direct/threads or Eio for `quartz.unix`, JS `Promise` for `quartz.edge_js`. Application code never names a concrete scheduler. | v4.0 |
| 11.2 | **Unicode, `Intl`, dates, plurals** | `Intl.DateTimeFormat`, `Intl.PluralRules`, `Intl.Collator`, and `toLocaleString` are built into every JS runtime. OCaml strings are bytes. | Use `uutf` / `uucp` / `uunf` for UTF-8 decoding, case mapping, and normalization. `Quartz.I18n` provides message catalogs and plural rules for the top ~20 locales (CLDR-generated table). In `quartz.browser` and `quartz.edge_js`, delegate formatting to native `Intl` through FFI. Store timestamps as UTC `int64` and use `ptime` for parsing/formatting. | v4.1 |
| 11.3 | **Image optimization** | `sharp` / Next `<Image>` resize and convert to AVIF/WebP on demand. These rely on native C libraries that cannot run inside a unikernel. | (a) Build-time pipeline: `quartz build` calls an external `libvips`/`imagemagick` binary if present to emit `srcset` variants. (b) Runtime: emit URLs for a CDN image service (Vercel Image, Cloudflare Images, imgproxy). (c) `Dom.img` always emits `width`/`height`/`loading="lazy"`/`decoding="async"`. Unikernels serve only pre-built variants. | v4.1 |
| 11.4 | **Regex and JSON in stdlib** | JS has built-in `RegExp` and `JSON.parse`. OCaml stdlib has neither (`Str` is legacy and not thread-safe). | Depend on `re` (pure OCaml, works on every target) for validation and routing patterns. Ship `Quartz.Json`, a small zero-dependency codec (~600 LOC) with a `ppx_deriving`-style typed encoder/decoder later; `yojson` adapter optional. | v4.0 |
| 11.5 | **Accessible UI primitives (shadcn/Radix)** | Thousands of tested React components with focus management and ARIA. | Build `Quartz.Headless`: `dialog` (native `<dialog>`), `popover` (Popover API + anchor positioning), `menu`, `tabs`, `combobox`, `listbox`, `tooltip`, `toast`, each following WAI-ARIA Authoring Practices, with roving tabindex and focus trap in `quartz.browser`. Styled defaults on top. `quartz import-html` converts markup and maps known patterns onto these primitives. Automated a11y checks in `quartz check` (roles, labels, focus order). | v4.0 core set, v4.2 full |
| 11.6 | **npm vendor SDKs** | Stripe, Clerk, Mapbox, PostHog ship JS first. | `Quartz.Js_bridge` + Custom Elements wrappers; generate bindings with `ts2ocaml` from `.d.ts` files instead of hand-writing them. Vendor widgets are client-only islands with a server-rendered placeholder/skeleton. | v4.1 |
| 11.7 | **Debugging experience** | Native JS objects in DevTools. | `--source-map` in dev builds; ppx-generated debug name table for variants and records; a `/__quartz/inspector` dev page showing live signals, route table, and request log; structured logs (`logs` library) with request IDs. | v4.1 |
| 11.8 | **Docs, examples, AI tooling, hiring** | Huge JS community and LLM training data. | `quartz new` starter templates (blog, SaaS dashboard, docs site, API-only); an `llms.txt` and a single-file API reference; example apps deployed per target; cookbook for each JS concept ("Next server actions in Quartz"). | Ongoing |
| 11.9 | **Security hardening** | Mature frameworks have years of CVE fixes. | Fuzz the HTTP parser, multipart parser, and cookie parser with `crowbar`/AFL; header and body size limits by default; auto-escaping in `Dom` with explicit `Dom.raw_unsafe`; CSRF + secure cookies on by default; dependency audit in CI. | v4.0 |
| 11.10 | **HTTP/2, HTTP/3, TLS on `quartz.unix`** | Node/Bun ship HTTP/2 and TLS. | Production deployments sit behind a TLS-terminating proxy (Fly, Cloud Run, Caddy, Vercel). Native TLS via `ocaml-tls`; HTTP/2 via `h2` as optional package. `quartz.mirage` uses `paf` (HTTP/1.1 + HTTP/2 + TLS). | v4.2 |

---

## 12. Roadmap to a Stable, Deployable Release (v4.0 LTS, 2026)

### 12.1 Release Phases

| Phase | Scope | Exit Criteria |
| :--- | :--- | :--- |
| **v4.0-alpha** | Split `quartz.core` / `quartz.unix`; `IO` signature; `Quartz.Json`; `re`; sessions + signed cookies; embedded store + `caqti` adapter; security defaults | `quartz.core` builds with **no** `unix` dependency; all existing v3 examples still pass `quartz check` |
| **v4.0-beta** | `quartz.edge_js` (Vercel/Cloudflare), `quartz.browser` islands with shared runtime chunk, `Quartz.Headless` core set, `quartz deploy vercel/fly/pages` | One example app deployed live to Vercel, Fly.io, and GitHub Pages from the same source; fuzzing runs in CI |
| **v4.0 stable** | `quartz.mirage` adapter; HMR; docs + templates; API freeze for `quartz.core` | Same app boots as a MirageOS unikernel on Solo5 `hvt`; Lighthouse >= 95; zero WCAG AA failures; semver guarantee |
| **v4.1** | I18n, image pipeline, `Js_bridge` + `ts2ocaml`, inspector, WebSockets rooms | Real-time demo + localized demo deployed |
| **v4.2** | Scene3d (WebGL2 first, WebGPU second), full Headless set, HTTP/2 | 3D landing-page demo at 60 fps on mid-range hardware |

### 12.2 Definition of "Stable"

1. **API freeze** of `quartz.core` public signatures, with semver from v4.0 on.
2. **Same source, 4 targets** proven in CI on every commit: Unix/Docker, Vercel (edge_js),
   GitHub Pages (static), MirageOS (`hvt`).
3. **Measured budgets enforced in CI:** server p99 latency, client JS per page,
   edge bundle size, unikernel image size.
4. **Security baseline:** fuzzed parsers, default CSRF/secure cookies/CSP headers,
   no `Dom.raw_unsafe` in templates.
5. **Docs:** getting started in under 5 minutes with `quartz new`, plus a migration guide for
   Next/Express developers.

### 12.3 Honest Bottom Line

A stable, full-featured, standalone Quartz deployable in 2026 is **realistic**:
nothing in this plan requires inventing new technology, and every piece (js_of_ocaml,
wasm_of_ocaml, MirageOS, `paf`, `caqti`, `re`, `uutf`, `ptime`) already exists and is maintained.
The real constraints are **engineering time** (~17-25k LOC, 9-12 months for 1-2
maintainers) and **ecosystem size**, not technical feasibility. What Quartz will not do
is match npm's breadth natively; it bridges to it instead.

