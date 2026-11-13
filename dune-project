(lang dune 2.9)
(name quartz)

(generate_opam_files true)

(authors "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(maintainers "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(homepage "https://github.com/cryogenicdeadfrost/quartz")
(bug_reports "https://github.com/cryogenicdeadfrost/quartz/issues")
(source (github cryogenicdeadfrost/quartz))
(license MIT)

(package
 (name quartz)
 (synopsis "Fine-grained reactive web LiveView, SSR, and Flexbox TUI framework in pure OCaml")
 (description "Quartz brings React/SolidJS ergonomics and Tailwind-style type-safe styling to OCaml with zero npm or JavaScript bundler bloat. Write declarative components once with fine-grained Signals and mount them as an interactive WebSocket LiveView web app, static HTML/CSS site, or 60fps ANSI terminal UI.")
 (depends
  (ocaml (>= 4.13.0))
  (dune (>= 2.9))))
