(lang dune 2.9)
(name quartz)
(version 4.0.0)

(generate_opam_files true)

(authors "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(maintainers "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(homepage "https://github.com/cryogenicdeadfrost/quartz")
(bug_reports "https://github.com/cryogenicdeadfrost/quartz/issues")
(source (github cryogenicdeadfrost/quartz))
(license MIT)

(package
 (name quartz)
 (synopsis "Full-stack reactive UI, WCAG styling, middleware server, and custom renderer engine in pure OCaml")
 (description "Quartz brings React/Svelte ergonomics, Tailwind-style type-safe CSS with WCAG 2.1 contrast math, Express/NestJS-style middleware, ISR/SSE, and multi-target custom renderers (LiveView HTML, Static SSG, Flexbox TUI, Canvas2D, Markdown, JSON Scenegraph) to OCaml with zero npm bloat.")
 (depends
  (ocaml (>= 4.13.0))
  (dune (>= 2.9))))
