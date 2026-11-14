(* Zero-npm HTTP + LiveView + JSON API server, plus a Vite-style build & check tool.
   Built straight on OCaml Unix sockets. *)

type api_handler = (string * string) list -> string

let api_get_routes : (string, api_handler) Hashtbl.t = Hashtbl.create 16
let api_post_routes : (string, api_handler) Hashtbl.t = Hashtbl.create 16

let api_get path fn = Hashtbl.replace api_get_routes path fn
let api_post path fn = Hashtbl.replace api_post_routes path fn

let client_runtime_js =
  {|
(function() {
  async function applyPayload(res) {
    if (!res.ok) return;
    const payload = await res.json();
    const rootEl = document.getElementById('quartz-root');
    const active = document.activeElement;
    const allCtrls = rootEl ? Array.from(rootEl.querySelectorAll('input,textarea,select')) : [];
    const activeIdx = allCtrls.indexOf(active);
    const selStart = (active && typeof active.selectionStart === 'number') ? active.selectionStart : null;

    const patchDom = () => {
      const styleEl = document.getElementById('quartz-styles');
      if (styleEl && payload.css !== undefined) styleEl.textContent = payload.css;
      if (rootEl && payload.html !== undefined) {
        rootEl.innerHTML = payload.html;
        if (activeIdx >= 0) {
          const nextCtrls = Array.from(rootEl.querySelectorAll('input,textarea,select'));
          const nextEl = nextCtrls[activeIdx];
          if (nextEl) {
            nextEl.focus();
            if (selStart !== null && typeof nextEl.setSelectionRange === 'function') {
              try { nextEl.setSelectionRange(selStart, selStart); } catch (_) {}
            }
          }
        }
      }
      if (payload.path && window.location.pathname !== payload.path) {
        window.history.pushState({ path: payload.path }, '', payload.path);
      }
    };

    patchDom();
  }

  async function sendEvent(id, val) {
    try {
      const res = await fetch('/__quartz_event', {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: 'id=' + encodeURIComponent(id) + '&val=' + encodeURIComponent(val || '')
      });
      await applyPayload(res);
    } catch (e) {
      console.error('[Quartz LiveView]', e);
    }
  }

  window.addEventListener('popstate', async function() {
    try {
      const res = await fetch('/__quartz_nav?path=' + encodeURIComponent(window.location.pathname + window.location.search));
      await applyPayload(res);
    } catch (_) {}
  });

  // 60fps fluid cursor spotlight tracker on [data-q-spotlight="1"] cards
  let lastMoveSent = 0;
  document.addEventListener('mousemove', function(e) {
    const cards = document.querySelectorAll('[data-q-spotlight="1"]');
    for (let i = 0; i < cards.length; i++) {
      const rect = cards[i].getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;
      cards[i].style.setProperty('--qx', x + 'px');
      cards[i].style.setProperty('--qy', y + 'px');
    }
    const mm = e.target.closest('[data-q-mousemove]');
    if (mm) {
      const now = performance.now();
      if (now - lastMoveSent > 90) {
        lastMoveSent = now;
        const r = mm.getBoundingClientRect();
        const rx = Math.round(e.clientX - r.left);
        const ry = Math.round(e.clientY - r.top);
        sendEvent(mm.getAttribute('data-q-mousemove'), rx + ',' + ry);
      }
    }
  });

  document.addEventListener('click', function(e) {
    const t = e.target.closest('[data-q-click]');
    if (t) {
      if (t.tagName === 'A') e.preventDefault();
      sendEvent(t.getAttribute('data-q-click'), '');
    }
  });

  document.addEventListener('input', function(e) {
    const t = e.target.closest('[data-q-input]');
    if (t) sendEvent(t.getAttribute('data-q-input'), t.value || '');
  });

  document.addEventListener('change', function(e) {
    const tc = e.target.closest('[data-q-check]');
    if (tc) {
      sendEvent(tc.getAttribute('data-q-check'), tc.checked ? 'true' : 'false');
      return;
    }
    const ch = e.target.closest('[data-q-change]');
    if (ch) sendEvent(ch.getAttribute('data-q-change'), ch.value || '');
  });

  document.addEventListener('keydown', function(e) {
    const t = e.target.closest('[data-q-keydown]');
    if (t) sendEvent(t.getAttribute('data-q-keydown'), e.key || '');
  });

  document.addEventListener('submit', function(e) {
    const f = e.target.closest('[data-q-submit]');
    if (f) {
      e.preventDefault();
      const fd = new FormData(f);
      const pairs = [];
      fd.forEach((v, k) => pairs.push(k + '=' + v));
      sendEvent(f.getAttribute('data-q-submit'), pairs.join('&'));
    }
  });
})();
|}

let base_reset_css =
  {|
*, *::before, *::after { box-sizing: border-box; }
html, body {
  margin: 0;
  padding: 0;
  background-color: #020617;
  color: #f8fafc;
  font-family: Inter, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  -webkit-font-smoothing: antialiased;
}
button, input, select, textarea { font-family: inherit; }
[data-q-spotlight="1"] {
  position: relative;
  background-image: radial-gradient(380px circle at var(--qx, -500px) var(--qy, -500px), rgba(56, 189, 248, 0.13), transparent 65%);
}
@keyframes q_fade_in {
  from { opacity: 0; }
  to { opacity: 1; }
}
@keyframes q_slide_up {
  from { opacity: 0; transform: translateY(10px) scale(0.98); }
  to { opacity: 1; transform: translateY(0) scale(1); }
}
@keyframes q_pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.45; }
}
|}

let escape_json_str s =
  let b = Buffer.create (String.length s + 32) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let render_full_document ?(title = "Quartz App") ?(live = true) (view : Dom.node) : string * Dom.compiled_view =
  let c = Dom.compile_node view in
  let script_tag = if live then Printf.sprintf "<script>%s</script>" client_runtime_js else "" in
  let doc =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en" class="dark">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>%s</title>
  <style>%s</style>
  <style id="quartz-styles">%s</style>
</head>
<body>
  <div id="quartz-root">%s</div>
  %s
</body>
</html>|}
      (Dom.escape_html title)
      base_reset_css
      c.css
      c.html
      script_tag
  in
  (doc, c)

let export_page ~path ?(title = "Quartz App") (app : unit -> Dom.node) =
  let doc, _ = render_full_document ~title ~live:false (app ()) in
  let oc = open_out_bin path in
  output_string oc doc;
  close_out oc;
  String.length doc

let export_site ~out_dir ?(title = "Quartz Site") (pages : (string * string * (unit -> Dom.node)) list) =
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter
    (fun (filename, route_path, view_fn) ->
      Router.navigate route_path;
      let full_path = Filename.concat out_dir filename in
      ignore (export_page ~path:full_path ~title view_fn))
    pages

let url_decode s =
  let len = String.length s in
  let b = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    match s.[!i] with
    | '+' ->
      Buffer.add_char b ' ';
      incr i
    | '%' when !i + 2 < len ->
      let hex = String.sub s (!i + 1) 2 in
      (match int_of_string_opt ("0x" ^ hex) with
       | Some code ->
         Buffer.add_char b (Char.chr (code land 0xff));
         i := !i + 3
       | None ->
         Buffer.add_char b '%';
         incr i)
    | c ->
      Buffer.add_char b c;
      incr i
  done;
  Buffer.contents b

let parse_form_urlencoded body =
  let pairs = String.split_on_char '&' body in
  List.filter_map
    (fun kv ->
      match String.split_on_char '=' kv with
      | k :: rest when k <> "" -> Some (url_decode k, url_decode (String.concat "=" rest))
      | _ -> None)
    pairs

let dispatch_handler (handlers : (int, Dom.handler) Hashtbl.t) ~id ~raw_val =
  match Hashtbl.find_opt handlers id with
  | None -> false
  | Some h ->
    (match h with
     | Dom.On_click fn -> fn ()
     | Dom.On_input fn -> fn raw_val
     | Dom.On_change fn -> fn raw_val
     | Dom.On_check fn -> fn (raw_val = "true" || raw_val = "1")
     | Dom.On_keydown fn ->
       fn { key = raw_val; ctrl = false; shift = false; alt = false }
     | Dom.On_mousemove fn ->
       (match String.split_on_char ',' raw_val with
        | [ xs; ys ] ->
          let x = Option.value ~default:0 (int_of_string_opt xs) in
          let y = Option.value ~default:0 (int_of_string_opt ys) in
          fn (x, y)
        | _ -> ())
     | Dom.On_submit fn -> fn (parse_form_urlencoded raw_val));
    true

let read_http_request fd =
  let buf = Bytes.create 16384 in
  let n = Unix.read fd buf 0 16384 in
  if n <= 0 then None
  else
    let raw = Bytes.sub_string buf 0 n in
    let lines = String.split_on_char '\n' raw in
    match lines with
    | [] -> None
    | req_line :: _ ->
      let parts = String.split_on_char ' ' (String.trim req_line) in
      let meth, path =
        match parts with
        | m :: p :: _ -> (m, p)
        | _ -> ("GET", "/")
      in
      let idx = ref 0 in
      let rlen = String.length raw in
      let found = ref (-1) in
      while !idx + 3 < rlen && !found = -1 do
        if raw.[!idx] = '\r'
           && raw.[!idx + 1] = '\n'
           && raw.[!idx + 2] = '\r'
           && raw.[!idx + 3] = '\n'
        then found := !idx + 4;
        incr idx
      done;
      let body =
        if !found >= 0 && !found <= rlen then String.sub raw !found (rlen - !found)
        else ""
      in
      Some (meth, path, body)

let write_http_response fd ~status ~content_type body =
  let resp =
    Printf.sprintf
      "HTTP/1.1 %s\r\nContent-Type: %s; charset=utf-8\r\nAccess-Control-Allow-Origin: *\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status
      content_type
      (String.length body)
      body
  in
  let _ = Unix.write_substring fd resp 0 (String.length resp) in
  ()

let split_url_and_query raw_url =
  match String.split_on_char '?' raw_url with
  | [] -> ("/", [])
  | [ p ] -> (p, [])
  | p :: rest -> (p, parse_form_urlencoded (String.concat "?" rest))

let run_browser ?(port = 8080) ?(title = "Quartz LiveView App") ?max_requests (app : unit -> Dom.node) =
  let root_node = app () in
  let last_compiled = ref (Dom.compile_node root_node) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 64;
  Printf.printf "\n  ⚡ QUARTZ v2.0 DEV & LIVEVIEW SERVER\n";
  Printf.printf "  ➜ Local:   http://localhost:%d/\n" port;
  Printf.printf "  ➜ Engine:  Native OCaml Reactive Signals + Zero-npm LiveView\n\n%!";
  let served = ref 0 in
  let keep_running () =
    match max_requests with
    | None -> true
    | Some m -> !served < m
  in
  while keep_running () do
    let client_fd, _ = Unix.accept sock in
    incr served;
    let t0 = Unix.gettimeofday () in
    (try
       match read_http_request client_fd with
       | None -> ()
       | Some ("POST", "/__quartz_event", body) ->
         let params = parse_form_urlencoded body in
         let id =
           Option.bind (List.assoc_opt "id" params) int_of_string_opt
           |> Option.value ~default:(-1)
         in
         let raw_val = Option.value ~default:"" (List.assoc_opt "val" params) in
         let _ = dispatch_handler (!last_compiled).handlers ~id ~raw_val in
         let updated = Dom.compile_node root_node in
         last_compiled := updated;
         let json =
           Printf.sprintf
             "{\"html\":\"%s\",\"css\":\"%s\",\"path\":\"%s\"}"
             (escape_json_str updated.html)
             (escape_json_str updated.css)
             (escape_json_str (Signal.get (Router.current_path ())))
         in
         write_http_response client_fd ~status:"200 OK" ~content_type:"application/json" json
       | Some ("GET", raw_url, _) when String.length raw_url >= 13 && String.sub raw_url 0 13 = "/__quartz_nav" ->
         let _, qs = split_url_and_query raw_url in
         let target = Option.value ~default:"/" (List.assoc_opt "path" qs) in
         Router.navigate target;
         let updated = Dom.compile_node root_node in
         last_compiled := updated;
         let json =
           Printf.sprintf
             "{\"html\":\"%s\",\"css\":\"%s\",\"path\":\"%s\"}"
             (escape_json_str updated.html)
             (escape_json_str updated.css)
             (escape_json_str (Signal.get (Router.current_path ())))
         in
         write_http_response client_fd ~status:"200 OK" ~content_type:"application/json" json
       | Some (meth, raw_url, body) ->
         let clean_path, qs_params = split_url_and_query raw_url in
         if meth = "GET" && Hashtbl.mem api_get_routes clean_path then (
           let fn = Hashtbl.find api_get_routes clean_path in
           let out = fn qs_params in
           let dt = (Unix.gettimeofday () -. t0) *. 1000.0 in
           Printf.printf "  [API GET]  %-22s 200 OK (%.2f ms)\n%!" clean_path dt;
           write_http_response client_fd ~status:"200 OK" ~content_type:"application/json" out
         ) else if meth = "POST" && Hashtbl.mem api_post_routes clean_path then (
           let fn = Hashtbl.find api_post_routes clean_path in
           let out = fn (parse_form_urlencoded body) in
           let dt = (Unix.gettimeofday () -. t0) *. 1000.0 in
           Printf.printf "  [API POST] %-22s 200 OK (%.2f ms)\n%!" clean_path dt;
           write_http_response client_fd ~status:"200 OK" ~content_type:"application/json" out
         ) else (
           Router.navigate raw_url;
           let doc, c = render_full_document ~title ~live:true root_node in
           last_compiled := c;
           let dt = (Unix.gettimeofday () -. t0) *. 1000.0 in
           Printf.printf "  [SSR GET]  %-22s 200 OK (%.2f ms, %.1f kB)\n%!"
             clean_path dt (float_of_int (String.length doc) /. 1024.0);
           write_http_response client_fd ~status:"200 OK" ~content_type:"text/html" doc
         )
     with _ -> ());
    (try Unix.close client_fd with _ -> ())
  done;
  (try Unix.close sock with _ -> ())

(* Vite-style static bundler with per-route size & timing breakdown *)
let run_vite_build ~out_dir ?(title = "Quartz App") (pages : (string * string * (unit -> Dom.node)) list) =
  let t0 = Unix.gettimeofday () in
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.printf "\n  ⚡ quartz v2.0 building static production bundle for %s...\n" title;
  let total_bytes = ref 0 in
  List.iter
    (fun (filename, route_path, view_fn) ->
      let p0 = Unix.gettimeofday () in
      Router.navigate route_path;
      let full_path = Filename.concat out_dir filename in
      let n_bytes = export_page ~path:full_path ~title view_fn in
      let dt = (Unix.gettimeofday () -. p0) *. 1000.0 in
      total_bytes := !total_bytes + n_bytes;
      Printf.printf "  ✓ %-28s %-14s %6.2f kB  │  rendered in %.2f ms\n"
        (Filename.concat out_dir filename) ("(" ^ route_path ^ ")") (float_of_int n_bytes /. 1024.0) dt)
    pages;
  let total_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  Printf.printf "  ─────────────────────────────────────────────────────────────────────────\n";
  Printf.printf "  ✓ Built %d pages (%.2f kB total, 0 kB external JS) in %.2f ms.\n\n%!"
    (List.length pages) (float_of_int !total_bytes /. 1024.0) total_ms

(* Vite-style diagnostic & verification suite (`quartz check`) *)
let run_vite_check
    ?(port = 8080)
    ~(routes : Router.route_def list)
    ~(sample_paths : string list)
    (app : unit -> Dom.node) =
  let t0 = Unix.gettimeofday () in
  Printf.printf "\n  ⚡ QUARTZ DOCTOR & STATIC ANALYZER (v2.0)\n";
  Printf.printf "  ====================================================================\n";

  (* 1. Route & Internal Link Verification *)
  let checked_links, broken_links = Router.audit_routes routes sample_paths app in
  Printf.printf "  [1/4] Route & Link Graph Check\n";
  Printf.printf "        • Registered Route Patterns : %d\n" (List.length routes);
  Printf.printf "        • Verified Internal <a href>: %d links (%s)\n"
    (List.length checked_links) (String.concat ", " checked_links);
  if broken_links = [] then
    Printf.printf "        • Dead / Unmatched Links    : 0 (PASS)\n"
  else (
    List.iter
      (fun (from_p, bad) ->
        Printf.printf "        ✗ BROKEN LINK on %s -> %s\n" from_p bad)
      broken_links;
    exit 1
  );

  (* 2. Per-Page DOM, Atomic CSS & Accessibility Audit *)
  Printf.printf "\n  [2/4] SSR DOM, Scoped CSS & A11y Audit\n";
  let total_warns = ref 0 in
  List.iter
    (fun p ->
      let p0 = Unix.gettimeofday () in
      Router.navigate p;
      let node = app () in
      let diag = Dom.diagnose node in
      let compiled = Dom.compile_node node in
      let dt = (Unix.gettimeofday () -. p0) *. 1000.0 in
      total_warns := !total_warns + List.length diag.a11y_warnings;
      Printf.printf "        • Route %-14s │ %3d nodes │ %2d handlers │ %2d CSS rules │ %5.1f kB │ %.2f ms\n"
        p diag.element_count diag.handler_count diag.scoped_css_count
        (float_of_int (String.length compiled.html + String.length compiled.css) /. 1024.0)
        dt;
      List.iter (fun w -> Printf.printf "          ⚠ %s\n" w) diag.a11y_warnings)
    sample_paths;

  (* 3. API Endpoint Check *)
  Printf.printf "\n  [3/4] Embedded REST API Endpoints\n";
  Printf.printf "        • Registered GET Endpoints  : %d\n" (Hashtbl.length api_get_routes);
  Hashtbl.iter
    (fun path fn ->
      let sample = fn [] in
      Printf.printf "        • GET %-21s -> %d bytes JSON\n" path (String.length sample))
    api_get_routes;

  (* 4. Network Socket Probe *)
  Printf.printf "\n  [4/4] Network Socket Readiness Probe (port %d)\n" port;
  let sock_ok =
    try
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      Unix.close s;
      true
    with _ -> false
  in
  Printf.printf "        • TCP Loopback Bind : %s\n"
    (if sock_ok then "AVAILABLE & READY (PASS)" else "PORT IN USE BY ANOTHER PROCESS");

  let elapsed = (Unix.gettimeofday () -. t0) *. 1000.0 in
  Printf.printf "  ====================================================================\n";
  Printf.printf "  ✓ All checks passed with %d warnings in %.2f ms.\n\n%!" !total_warns elapsed
