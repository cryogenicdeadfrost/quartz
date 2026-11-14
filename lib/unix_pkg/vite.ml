(* Vite-style Static Bundler (`quartz build`), 6-Stage WCAG + APCA + A11y Doctor (`quartz check`),
   Content-Hashed Asset Pipeline, /public Copier, llms.txt Emitter, and `Unix.stat` HMR Watcher. *)

open Quartz_core

let write_text_file path content =
  Deploy.ensure_dir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let copy_file ~src ~dst =
  Deploy.ensure_dir_p (Filename.dirname dst);
  let ic = open_in_bin src in
  let data =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  write_text_file dst data

let rec copy_dir_recursive ~src ~dst =
  if Sys.file_exists src && Sys.is_directory src then (
    Deploy.ensure_dir_p dst;
    Array.iter
      (fun entry ->
        let sp = Filename.concat src entry in
        let dp = Filename.concat dst entry in
        if Sys.is_directory sp then copy_dir_recursive ~src:sp ~dst:dp
        else copy_file ~src:sp ~dst:dp)
      (Sys.readdir src)
  )

let run_vite_build
    ~out_dir
    ?(base_url = "https://quartz.ocaml.org")
    ?(title = "Quartz App")
    ?(public_dir = "public")
    (pages : (string * string * (unit -> Dom.node)) list) =
  let t0 = Unix.gettimeofday () in
  Deploy.ensure_dir_p out_dir;
  Deploy.ensure_dir_p (Filename.concat out_dir "assets");
  Printf.printf "\n  [quartz v4.0] building static production bundle + hashed runtime for %s...\n" title;

  (* Emit shared browser runtime chunk (content-hashed!) so islands share ONE chunk *)
  let runtime_js = Quartz_browser.Browser_runtime.client_runtime_js in
  let rt_hash = String.sub (Crypto_util.sha256_hex runtime_js) 0 8 in
  let rt_rel = Printf.sprintf "assets/quartz-runtime.%s.js" rt_hash in
  write_text_file (Filename.concat out_dir rt_rel) runtime_js;

  let base_css_hash = String.sub (Crypto_util.sha256_hex App.base_reset_css) 0 8 in
  let base_css_rel = Printf.sprintf "assets/styles.%s.css" base_css_hash in
  write_text_file (Filename.concat out_dir base_css_rel) App.base_reset_css;

  (* Copy /public/* if present *)
  if Sys.file_exists public_dir && Sys.is_directory public_dir then
    copy_dir_recursive ~src:public_dir ~dst:out_dir;

  let total_bytes = ref 0 in
  let manifest_entries = ref [] in
  List.iter
    (fun (filename, route_path, view_fn) ->
      let p0 = Unix.gettimeofday () in
      Router.navigate route_path;
      let node = view_fn () in
      let full_path = Filename.concat out_dir filename in
      let meta = Dom.{ (default_meta ~title ()) with canonical = Some (base_url ^ route_path) } in
      let n_bytes = Server.export_page ~path:full_path ~title ~meta (fun () -> node) in
      let dt = (Unix.gettimeofday () -. p0) *. 1000.0 in
      total_bytes := !total_bytes + n_bytes;
      manifest_entries :=
        Json.obj
          [
            ("route", Json.string route_path);
            ("file", Json.string filename);
            ("bytes", Json.int n_bytes);
          ]
        :: !manifest_entries;
      Printf.printf "  [OK] %-28s %-14s %6.2f kB  |  rendered in %.2f ms\n"
        full_path ("(" ^ route_path ^ ")") (float_of_int n_bytes /. 1024.0) dt)
    pages;

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
    (Json.to_pretty_string
       (Json.obj
          [
            ("engine", Json.string "quartz-v4");
            ("runtimeAsset", Json.string rt_rel);
            ("baseCssAsset", Json.string base_css_rel);
            ("pages", Json.array (List.rev !manifest_entries));
          ]));
  let llms_routes =
    List.map (fun (_, p, _) -> Router.route ~title:p p (fun _ -> Dom.empty)) pages
  in
  write_text_file (Filename.concat out_dir "llms.txt")
    (Deploy.generate_llms_txt ~title ~base_url llms_routes);

  let total_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  Printf.printf "  [OK] %-28s %-14s %6.2f kB  |  shared islands + Scene3d runtime\n"
    rt_rel "(JS Runtime)" (float_of_int (String.length runtime_js) /. 1024.0);
  Printf.printf "  [OK] %-28s %-14s %6.2f kB  |  generated SEO, manifest & llms.txt\n"
    (Filename.concat out_dir "sitemap.xml + llms.txt") "(SEO/AI)"
    (float_of_int (String.length sitemap_xml) /. 1024.0);
  Printf.printf "  -------------------------------------------------------------------------\n";
  Printf.printf "  [OK] Built %d pages + hashed assets (%.2f kB HTML total) in %.2f ms.\n\n%!"
    (List.length pages) (float_of_int !total_bytes /. 1024.0) total_ms

let run_vite_check
    ?(port = 8080)
    ?(max_kb = 64.0)
    ~(routes : Router.route_def list)
    ~(sample_paths : string list)
    (app : unit -> Dom.node) =
  let t0 = Unix.gettimeofday () in
  Printf.printf "\n  QUARTZ v4.0 DOCTOR, WCAG/APCA AUDITOR & STATIC ANALYZER\n";
  Printf.printf "  ====================================================================\n";

  (* 1. Route & Internal Link Verification *)
  let checked_links, broken_links = Router.audit_routes routes sample_paths app in
  Printf.printf "  [1/6] Route & Link Graph Check\n";
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

  (* 2. Per-Page DOM, Atomic CSS, A11y, Islands & Page Weight Budget Audit *)
  Printf.printf "\n  [2/6] SSR DOM, Scoped CSS, Islands & Page Budget (<= %.0f kB) Audit\n" max_kb;
  let total_warns = ref 0 in
  let total_contrast_checks = ref 0 in
  let total_contrast_warns = ref 0 in
  let total_apca_checks = ref 0 in
  let total_apca_warns = ref 0 in
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
      total_apca_checks := !total_apca_checks + diag.apca_checks;
      total_apca_warns := !total_apca_warns + List.length diag.apca_warnings;
      if kb > max_kb then incr total_warns;
      Printf.printf "        * Route %-22s | %3d nodes | %2d hnd | %2d CSS | %5.1f kB | %.2f ms\n"
        p diag.element_count diag.handler_count diag.scoped_css_count kb dt;
      List.iter (fun w -> Printf.printf "          [WARN] %s\n" w) diag.a11y_warnings;
      List.iter (fun cw -> Printf.printf "          [WARN] [WCAG] %s\n" cw) diag.contrast_warnings)
    sample_paths;

  (* 3. WCAG 2.1 + APCA Color Contrast Audit *)
  Printf.printf "\n  [3/6] WCAG 2.1 & APCA Perceptual Luminance Audit\n";
  Printf.printf "        * Audited Color Pairs       : %d foreground/background pairs\n" !total_contrast_checks;
  Printf.printf "        * WCAG 2.1 Violations       : %d %s\n"
    !total_contrast_warns
    (if !total_contrast_warns = 0 then "(PASS)" else "(WARN)");
  Printf.printf "        * APCA Lc (<35) Warnings    : %d (%d pairs evaluated)\n"
    !total_apca_warns !total_apca_checks;

  (* 4. Accessibility & CLS Audit *)
  Printf.printf "\n  [4/6] Accessibility (WAI-ARIA, Labels, Alt) Audit\n";
  Printf.printf "        * Accessibility Warnings    : %d %s\n"
    !total_warns (if !total_warns = 0 then "(PASS)" else "(WARN)");

  (* 5. API Endpoint Check *)
  let apis = Server.get_api_routes () in
  Printf.printf "\n  [5/6] Embedded REST APIs & ISR Cache\n";
  Printf.printf "        * Registered GET Endpoints  : %d\n" (Hashtbl.length apis);
  Hashtbl.iter
    (fun path fn ->
      let sample = fn [] in
      Printf.printf "        * GET %-21s -> %d bytes JSON\n" path (String.length sample))
    apis;

  (* 6. Network Socket Probe *)
  Printf.printf "\n  [6/6] Network Socket Readiness Probe (port %d)\n" port;
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
  Printf.printf "  [OK] All 6 stages checked with %d warnings in %.2f ms.\n\n%!"
    (!total_warns + !total_contrast_warns) elapsed

(* HMR Directory Watcher using Unix.stat mtimes *)
let collect_mtimes ~exts dirs : (string, float) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  let has_ext f =
    List.exists (fun e -> Filename.check_suffix f e) exts
  in
  let rec walk dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Array.iter
        (fun entry ->
          if entry <> "_build" && entry <> ".git" && entry <> "dist_site" then
            let p = Filename.concat dir entry in
            try
              let st = Unix.stat p in
              if st.st_kind = Unix.S_DIR then walk p
              else if has_ext p then Hashtbl.replace tbl p st.st_mtime
            with _ -> ())
        (Sys.readdir dir)
  in
  List.iter walk dirs;
  tbl

let start_hmr_watcher ?(dirs = [ "lib"; "bin" ]) ?(poll_sec = 0.1) () =
  let baseline = ref (collect_mtimes ~exts:[ ".ml"; ".mli"; ".css" ] dirs) in
  let stop = ref false in
  let _th =
    Thread.create
      (fun () ->
        while not !stop do
          Unix.sleepf poll_sec;
          let cur = collect_mtimes ~exts:[ ".ml"; ".mli"; ".css" ] dirs in
          let changed = ref false in
          Hashtbl.iter
            (fun path mt ->
              match Hashtbl.find_opt !baseline path with
              | Some old_mt when old_mt = mt -> ()
              | _ -> changed := true)
            cur;
          if !changed then (
            baseline := cur;
            let rc = Sys.command "dune build" in
            if rc = 0 then (
              let n = Websocket.Rooms.broadcast ~room:"__hmr" "hmr_reload" in
              Printf.printf "  [HMR] Rebuilt cleanly -> notified %d browser client(s)\n%!" n
            )
          )
        done)
      ()
  in
  (fun () -> stop := true)
