(* Zero-npm HTTP + LiveView server & static HTML compiler.
   Uses standard OCaml Unix sockets so you never have to debug node_modules again. *)

let client_runtime_js =
  {|
(function() {
  async function sendEvent(id, val) {
    try {
      const active = document.activeElement;
      const activeTag = active ? active.tagName : null;
      const activeName = active ? active.getAttribute('name') || active.id : null;
      const selStart = (active && typeof active.selectionStart === 'number') ? active.selectionStart : null;

      const res = await fetch('/__quartz_event', {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: 'id=' + encodeURIComponent(id) + '&val=' + encodeURIComponent(val || '')
      });
      if (!res.ok) return;
      const payload = await res.json();
      const styleEl = document.getElementById('quartz-styles');
      if (styleEl && payload.css !== undefined) styleEl.textContent = payload.css;
      const rootEl = document.getElementById('quartz-root');
      if (rootEl && payload.html !== undefined) {
        rootEl.innerHTML = payload.html;
        if (activeName) {
          const nextEl = document.querySelector('[name="' + activeName + '"], #' + activeName);
          if (nextEl) {
            nextEl.focus();
            if (selStart !== null && typeof nextEl.setSelectionRange === 'function') {
              nextEl.setSelectionRange(selStart, selStart);
            }
          }
        }
      }
      if (payload.path && window.location.pathname !== payload.path) {
        window.history.pushState({}, '', payload.path);
      }
    } catch (e) {
      console.error('[Quartz LiveView]', e);
    }
  }

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
  close_out oc

let export_site ~out_dir ?(title = "Quartz Site") (pages : (string * string * (unit -> Dom.node)) list) =
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter
    (fun (filename, route_path, view_fn) ->
      Router.navigate route_path;
      let full_path = Filename.concat out_dir filename in
      export_page ~path:full_path ~title view_fn)
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
      | k :: rest -> Some (url_decode k, url_decode (String.concat "=" rest))
      | [] -> None)
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
     | Dom.On_submit fn -> fn (parse_form_urlencoded raw_val));
    true

let read_http_request fd =
  let buf = Bytes.create 8192 in
  let n = Unix.read fd buf 0 8192 in
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
      let body =
        match String.split_on_char '\r' raw with
        | _ ->
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
          if !found >= 0 && !found <= rlen then String.sub raw !found (rlen - !found)
          else ""
      in
      Some (meth, path, body)

let write_http_response fd ~status ~content_type body =
  let resp =
    Printf.sprintf
      "HTTP/1.1 %s\r\nContent-Type: %s; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status
      content_type
      (String.length body)
      body
  in
  let _ = Unix.write_substring fd resp 0 (String.length resp) in
  ()

let run_browser ?(port = 8080) ?(title = "Quartz LiveView App") ?max_requests (app : unit -> Dom.node) =
  let root_node = app () in
  let last_compiled = ref (Dom.compile_node root_node) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 32;
  Printf.printf "⚡ Quartz LiveView running at http://localhost:%d\n%!" port;
  let served = ref 0 in
  let keep_running () =
    match max_requests with
    | None -> true
    | Some m -> !served < m
  in
  while keep_running () do
    let client_fd, _ = Unix.accept sock in
    incr served;
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
       | Some (_, path, _) ->
         Router.navigate path;
         let doc, c = render_full_document ~title ~live:true root_node in
         last_compiled := c;
         write_http_response client_fd ~status:"200 OK" ~content_type:"text/html" doc
     with _ -> ());
    (try Unix.close client_fd with _ -> ())
  done;
  (try Unix.close sock with _ -> ())
