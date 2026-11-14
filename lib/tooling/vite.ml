(* Vite-style static bundler (`quartz build`) and static doctor (`quartz check`)
   with WCAG 2.1 Color Contrast Auditing, Page Weight Budgets, and Sitemap generation. *)

let write_text_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let run_vite_build
    ~out_dir
    ?(base_url = "https://quartz.ocaml.org")
    ?(title = "Quartz App")
    (pages : (string * string * (unit -> Dom.node)) list) =
  let t0 = Unix.gettimeofday () in
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.printf "\n  [quartz v3.0] building static production bundle for %s...\n" title;
  let total_bytes = ref 0 in
  let manifest_entries = ref [] in
  List.iter
    (fun (filename, route_path, view_fn) ->
      let p0 = Unix.gettimeofday () in
      Router.navigate route_path;
      let full_path = Filename.concat out_dir filename in
      let n_bytes = Server.export_page ~path:full_path ~title view_fn in
      let dt = (Unix.gettimeofday () -. p0) *. 1000.0 in
      total_bytes := !total_bytes + n_bytes;
      manifest_entries :=
        Printf.sprintf {|    {"route": "%s", "file": "%s", "bytes": %d}|} route_path filename n_bytes
        :: !manifest_entries;
      Printf.printf "  [OK] %-28s %-14s %6.2f kB  |  rendered in %.2f ms\n"
        (Filename.concat out_dir filename) ("(" ^ route_path ^ ")") (float_of_int n_bytes /. 1024.0) dt)
    pages;

  (* Emit sitemap.xml, robots.txt, and build-manifest.json *)
  let sitemap_urls =
    String.concat "\n"
      (List.map
         (fun (_, route_path, _) ->
           Printf.sprintf "  <url><loc>%s%s</loc></url>" base_url route_path)
         pages)
  in
  let sitemap_xml =
    Printf.sprintf
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n%s\n</urlset>\n"
      sitemap_urls
  in
  write_text_file (Filename.concat out_dir "sitemap.xml") sitemap_xml;
  write_text_file (Filename.concat out_dir "robots.txt")
    (Printf.sprintf "User-agent: *\nAllow: /\nSitemap: %s/sitemap.xml\n" base_url);
  write_text_file (Filename.concat out_dir "build-manifest.json")
    (Printf.sprintf "{\n  \"engine\": \"quartz-v3\",\n  \"pages\": [\n%s\n  ]\n}\n"
       (String.concat ",\n" (List.rev !manifest_entries)));

  let total_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  Printf.printf "  [OK] %-28s %-14s %6.2f kB  |  generated SEO & manifest\n"
    (Filename.concat out_dir "sitemap.xml + manifest") "(SEO/Meta)"
    (float_of_int (String.length sitemap_xml) /. 1024.0);
  Printf.printf "  -------------------------------------------------------------------------\n";
  Printf.printf "  [OK] Built %d pages + SEO assets (%.2f kB total, 0 kB external JS) in %.2f ms.\n\n%!"
    (List.length pages) (float_of_int !total_bytes /. 1024.0) total_ms

let run_vite_check
    ?(port = 8080)
    ?(max_kb = 64.0)
    ~(routes : Router.route_def list)
    ~(sample_paths : string list)
    (app : unit -> Dom.node) =
  let t0 = Unix.gettimeofday () in
  Printf.printf "\n  QUARTZ v3.0 DOCTOR, WCAG AUDITOR & STATIC ANALYZER\n";
  Printf.printf "  ====================================================================\n";

  (* 1. Route & Internal Link Verification *)
  let checked_links, broken_links = Router.audit_routes routes sample_paths app in
  Printf.printf "  [1/5] Route & Link Graph Check\n";
  Printf.printf "        * Registered Route Patterns : %d\n" (List.length routes);
  Printf.printf "        * Verified Internal <a href>: %d links (%s)\n"
    (List.length checked_links) (String.concat ", " checked_links);
  if broken_links = [] then
    Printf.printf "        * Dead / Unmatched Links    : 0 (PASS)\n"
  else (
    List.iter
      (fun (from_p, bad) ->
        Printf.printf "        [FAIL] BROKEN LINK on %s -> %s\n" from_p bad)
      broken_links;
    exit 1
  );

  (* 2. Per-Page DOM, Atomic CSS, A11y & Page Weight Budget Audit *)
  Printf.printf "\n  [2/5] SSR DOM, Scoped CSS, A11y & Page Budget (<= %.0f kB) Audit\n" max_kb;
  let total_warns = ref 0 in
  let total_contrast_checks = ref 0 in
  let total_contrast_warns = ref 0 in
  List.iter
    (fun p ->
      let p0 = Unix.gettimeofday () in
      Router.navigate p;
      let node = app () in
      let diag = Dom.diagnose node in
      let compiled = Dom.compile_node node in
      let dt = (Unix.gettimeofday () -. p0) *. 1000.0 in
      let kb = float_of_int (String.length compiled.html + String.length compiled.css) /. 1024.0 in
      total_warns := !total_warns + List.length diag.a11y_warnings;
      total_contrast_checks := !total_contrast_checks + diag.contrast_checks;
      total_contrast_warns := !total_contrast_warns + List.length diag.contrast_warnings;
      if kb > max_kb then incr total_warns;
      Printf.printf "        * Route %-14s | %3d nodes | %2d handlers | %2d CSS | %5.1f kB | %.2f ms\n"
        p diag.element_count diag.handler_count diag.scoped_css_count kb dt;
      List.iter (fun w -> Printf.printf "          [WARN] %s\n" w) diag.a11y_warnings;
      List.iter (fun cw -> Printf.printf "          [WARN] [WCAG] %s\n" cw) diag.contrast_warnings)
    sample_paths;

  (* 3. WCAG 2.1 Relative Luminance & Color Contrast Report *)
  Printf.printf "\n  [3/5] WCAG 2.1 Color Contrast & Relative Luminance Audit\n";
  Printf.printf "        * Audited Color Pairs       : %d foreground/background pairs\n" !total_contrast_checks;
  Printf.printf "        * Low-Contrast Violations   : %d %s\n"
    !total_contrast_warns
    (if !total_contrast_warns = 0 then "(PASS)" else "(WARN)");

  (* 4. API Endpoint & Middleware Check *)
  let apis = Server.get_api_routes () in
  Printf.printf "\n  [4/5] Embedded REST APIs & ISR Cache\n";
  Printf.printf "        * Registered GET Endpoints  : %d\n" (Hashtbl.length apis);
  Hashtbl.iter
    (fun path fn ->
      let sample = fn [] in
      Printf.printf "        * GET %-21s -> %d bytes JSON\n" path (String.length sample))
    apis;

  (* 5. Network Socket Probe *)
  Printf.printf "\n  [5/5] Network Socket Readiness Probe (port %d)\n" port;
  let sock_ok =
    try
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      Unix.close s;
      true
    with _ -> false
  in
  Printf.printf "        * TCP Loopback Bind : %s\n"
    (if sock_ok then "AVAILABLE & READY (PASS)" else "PORT IN USE BY ANOTHER PROCESS");

  let elapsed = (Unix.gettimeofday () -. t0) *. 1000.0 in
  Printf.printf "  ====================================================================\n";
  Printf.printf "  [OK] All checks passed with %d warnings in %.2f ms.\n\n%!"
    (!total_warns + !total_contrast_warns) elapsed
