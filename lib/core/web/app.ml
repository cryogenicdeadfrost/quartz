(* Universal Transport-Agnostic Full-Stack Application Functor (`Quartz.App`).
   Zero Unix dependency -- compiles identically to:
   - `quartz.unix` (Multi-threaded Keep-Alive + WebSocket + SSE)
   - `quartz.mirage` (MirageOS 4 Solo5 Unikernel)
   - `quartz.vercel` (Vercel Native Bootstrap / Lambda Runtime API)
   - `quartz.edge` (Vercel Edge / Cloudflare Workers / Deno Deploy JS/Wasm) *)

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
a:focus-visible, button:focus-visible, input:focus-visible, select:focus-visible {
  outline: 2px solid #38bdf8;
  outline-offset: 2px;
}
[data-q-spotlight="1"] {
  position: relative;
  background-image: radial-gradient(380px circle at var(--qx, -500px) var(--qy, -500px), rgba(56, 189, 248, 0.13), transparent 65%);
}
[data-q-magnetic="1"] {
  transform: translate3d(var(--mx, 0px), var(--my, 0px), 0);
  transition: transform 140ms cubic-bezier(0.16, 1, 0.3, 1);
  will-change: transform;
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
@media (prefers-reduced-motion: reduce) {
  *, *::before, *::after {
    animation-duration: 0.01ms !important;
    transition-duration: 0.01ms !important;
  }
}
|}

let render_full_document
    ?(title = "Quartz App")
    ?(meta : Dom.page_meta option)
    ?(live_script = "")
    ?(live = true)
    ?(external_css_href = "")
    ?(external_js_src = "")
    (view : Dom.node) : string * Dom.compiled_view =
  let c = Dom.compile_node view in
  let effective_meta =
    match meta with
    | Some m -> m
    | None -> Dom.default_meta ~title ()
  in
  let meta_html = Dom.render_meta_tags effective_meta in
  let ext_css_tag =
    if external_css_href <> "" then
      Printf.sprintf "\n  <link rel=\"stylesheet\" href=\"%s\" />\n" (Dom.escape_html external_css_href)
    else ""
  in
  let script_tag =
    let parts = ref [] in
    if external_js_src <> "" then
      parts := Printf.sprintf "<script defer src=\"%s\"></script>" (Dom.escape_html external_js_src) :: !parts;
    if live && live_script <> "" then
      parts := Printf.sprintf "<script>%s</script>" live_script :: !parts;
    String.concat "\n  " (List.rev !parts)
  in
  let doc =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="%s" class="dark">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>%s</title>
%s%s  <style>%s</style>
  <style id="quartz-styles">%s</style>
</head>
<body>
  <div id="quartz-root">%s</div>
  %s
</body>
</html>|}
      (Dom.escape_html effective_meta.lang)
      (Dom.escape_html effective_meta.title)
      meta_html
      ext_css_tag
      base_reset_css
      c.css
      c.html
      script_tag
  in
  (doc, c)

(* Streaming SSR / Out-of-Order Suspense Chunks (shell first, then deferred slots) *)
let render_streaming_document
    ?(title = "Quartz App")
    ?(meta : Dom.page_meta option)
    ~(shell : Dom.node)
    (deferred_slots : (string * (unit -> Dom.node)) list) : string list =
  let shell_c = Dom.compile_node shell in
  let effective_meta =
    match meta with
    | Some m -> m
    | None -> Dom.default_meta ~title ()
  in
  let meta_html = Dom.render_meta_tags effective_meta in
  let head_chunk =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="%s" class="dark">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>%s</title>
%s  <style>%s</style>
  <style id="quartz-styles">%s</style>
</head>
<body>
  <div id="quartz-root">%s</div>|}
      (Dom.escape_html effective_meta.lang)
      (Dom.escape_html effective_meta.title)
      meta_html
      base_reset_css
      shell_c.css
      shell_c.html
  in
  let slot_chunks =
    List.map
      (fun (slot_id, build_node) ->
        let c = Dom.compile_node (build_node ()) in
        let safe_id = Dom.escape_html slot_id in
        Printf.sprintf
          "<style>%s</style><template id=\"q-s-%s\">%s</template><script>(function(){var s=document.getElementById(%S),t=document.getElementById(%S);if(s&&t)s.innerHTML=t.innerHTML;})();</script>"
          c.css safe_id c.html slot_id ("q-s-" ^ safe_id))
      deferred_slots
  in
  (head_chunk :: slot_chunks) @ [ "</body>\n</html>" ]

let dispatch_handler (handlers : (int, Dom.handler) Hashtbl.t) ~id ~raw_val =
  match Hashtbl.find_opt handlers id with
  | None -> false
  | Some h ->
    Signal.batch (fun () ->
      match h with
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
      | Dom.On_submit fn -> fn (Http.parse_form_urlencoded raw_val));
    true

module Make
    (C : Io.CLOCK)
    (S : Io.STORAGE with type 'a io = 'a)
    (P : Io.PUBSUB) =
struct
  module Sessions = Cookie.Session.Make (C) (S)

  type api_handler = Middleware.req -> Middleware.res

  type config = {
    title : string;
    session_secret : string;
    session_ttl_s : int;
    secure_cookies : bool;
    client_script : string;
    routes : Router.route_def list;
    redirects : Router.redirect_def list;
    middlewares : Middleware.t list;
    rpcs : Middleware.Endpoint.boxed list;
    get_apis : (string * api_handler) list;
    post_apis : (string * api_handler) list;
    static_assets : (string * string * string) list; (* path, content_type, body *)
    render_for_session : Cookie.Session.session_data -> Middleware.req -> Dom.node;
  }

  type visitor_view_state = {
    mutable compiled : Dom.compiled_view;
    mutable view_builder : unit -> Dom.node;
  }

  type t = {
    cfg : config;
    sessions : Sessions.t;
    visitor_views : (string, visitor_view_state) Hashtbl.t;
    isr_cache : (string, string * float) Hashtbl.t;
  }

  let create (cfg : config) : t =
    {
      cfg;
      sessions =
        Sessions.create_manager
          ~ttl_s:cfg.session_ttl_s
          ~secure:cfg.secure_cookies
          ~secret:cfg.session_secret
          ();
      visitor_views = Hashtbl.create 64;
      isr_cache = Hashtbl.create 32;
    }

  let json_response ?(use_diff = false) ~(prev : Dom.compiled_view option) (next : Dom.compiled_view) (path : string) =
    let patches_opt =
      match use_diff, prev with
      | true, Some old_v ->
        let ps = Dom.diff old_v.snapshot next.snapshot in
        (* Use granular patches if small; full html when patches are empty or complex *)
        if ps = [] then Some (Dom.patches_to_json [])
        else if List.length ps <= 12 then Some (Dom.patches_to_json ps)
        else None
      | _ -> None
    in
    let fields =
      [
        ("css", Json.string next.css);
        ("path", Json.string path);
      ]
      @
      match patches_opt with
      | Some pj -> [ ("patches", pj) ]
      | None -> [ ("html", Json.string next.html) ]
    in
    Middleware.ok (Json.to_string (Json.obj fields))

  let serve_static (req : Middleware.req) _path ct content =
    let total = String.length content in
    let etag = "\"" ^ String.sub (Crypto_util.sha256_hex content) 0 16 ^ "\"" in
    match Middleware.header_opt "if-none-match" req with
    | Some ctag when String.trim ctag = etag ->
      { Middleware.status = "304 Not Modified"; content_type = ct; headers = [ ("ETag", etag) ]; body = "" }
    | _ ->
      (match Middleware.header_opt "range" req with
       | Some rng ->
         (match Http.parse_range ~total_len:total rng with
          | Some (s, e) ->
            let len = e - s + 1 in
            let slice = String.sub content s len in
            {
              Middleware.status = "206 Partial Content";
              content_type = ct;
              headers =
                [
                  ("Accept-Ranges", "bytes");
                  ("Content-Range", Printf.sprintf "bytes %d-%d/%d" s e total);
                  ("ETag", etag);
                ];
              body = slice;
            }
          | None ->
            Middleware.reject ~status:"416 Range Not Satisfiable" "Invalid byte range")
       | None ->
         Middleware.ok ~content_type:ct
           ~headers:[ ("Accept-Ranges", "bytes"); ("ETag", etag); ("Cache-Control", "public, max-age=3600") ]
           content)

  let handle (app : t) (req : Middleware.req) : Middleware.res =
    try
      (* 1. Static asset map check (works in Mirage crunch & Edge bundles) *)
      match
        List.find_opt (fun (p, _, _) -> p = req.path) app.cfg.static_assets
      with
      | Some (_, ct, body) when req.meth = "GET" || req.meth = "HEAD" ->
        let r = serve_static req req.path ct body in
        if req.meth = "HEAD" then { r with body = "" } else r
      | _ ->
        let cookie_hdr = Option.value ~default:"" (Middleware.header_opt "cookie" req) in
        let session, is_new_session = Sessions.load_or_create app.sessions ~cookie_header:cookie_hdr in
        let attach_session_cookie res =
          if is_new_session then
            Middleware.add_header "Set-Cookie" (Sessions.set_cookie_header app.sessions session) res
          else res
        in
        let core_handler (r : Middleware.req) : Middleware.res =
          (* 2. Cron webhook trigger *)
          let cron_prefix = "/api/__cron/" in
          let cplen = String.length cron_prefix in
          if String.length r.path > cplen && String.sub r.path 0 cplen = cron_prefix then (
            let cname = String.sub r.path cplen (String.length r.path - cplen) in
            match Jobs.trigger_cron_webhook ~name:cname with
            | None -> Middleware.not_found ~content_type:"application/json" {|{"error":"Unknown cron job"}|}
            | Some (Ok ()) ->
              P.publish ~topic:"cron" ("ran:" ^ cname);
              Middleware.ok (Json.to_string (Json.obj [ ("cron", Json.string cname); ("ok", Json.bool true) ]))
            | Some (Error msg) -> Middleware.reject ~status:"500 Internal Server Error" msg
          ) else
          (* 3. Typed RPC endpoints *)
          match List.find_opt (fun (b : Middleware.Endpoint.boxed) -> b.b_path = r.path && b.b_meth = r.meth) app.cfg.rpcs with
          | Some box -> box.dispatch r
          | None ->
            (* 4. LiveView Event (`/__quartz_event`) with per-visitor isolation + DOM diff *)
            if r.meth = "POST" && r.path = "/__quartz_event" then (
              let params = Http.parse_form_urlencoded r.body in
              let id =
                Option.bind (List.assoc_opt "id" params) int_of_string_opt
                |> Option.value ~default:(-1)
              in
              let raw_val = Option.value ~default:"" (List.assoc_opt "val" params) in
              let vst =
                match Hashtbl.find_opt app.visitor_views session.sid with
                | Some v -> v
                | None ->
                  let builder () = app.cfg.render_for_session session r in
                  let v = { compiled = Dom.compile_node (builder ()); view_builder = builder } in
                  Hashtbl.replace app.visitor_views session.sid v;
                  v
              in
              let prev_c = vst.compiled in
              ignore (dispatch_handler prev_c.handlers ~id ~raw_val);
              let next_c = Dom.compile_node (vst.view_builder ()) in
              vst.compiled <- next_c;
              Sessions.save app.sessions session;
              json_response ~use_diff:true ~prev:(Some prev_c) next_c (Signal.get (Router.current_path ()))
            ) else if r.meth = "GET" && r.path = "/__quartz_nav" then (
              let target = Option.value ~default:"/" (List.assoc_opt "path" r.query) in
              Router.navigate target;
              let view = app.cfg.render_for_session session r in
              let next_c = Dom.compile_node view in
              Hashtbl.replace app.visitor_views session.sid { compiled = next_c; view_builder = (fun () -> app.cfg.render_for_session session r) };
              json_response ~use_diff:false ~prev:None next_c (Signal.get (Router.current_path ()))
            ) else if r.meth = "GET" && List.mem_assoc r.path app.cfg.get_apis then (
              let fn = List.assoc r.path app.cfg.get_apis in
              fn r
            ) else if r.meth = "POST" && List.mem_assoc r.path app.cfg.post_apis then (
              let fn = List.assoc r.path app.cfg.post_apis in
              fn r
            ) else (
              (* 5. Full SSR Router with real HTTP 200 / 301 / 302 / 404 status codes *)
              let resolution = Router.resolve ~redirects:app.cfg.redirects app.cfg.routes r.path in
              match resolution with
              | Router.Redirected { status_code; location } ->
                let st_str = if status_code = 301 then "301 Moved Permanently" else "302 Found" in
                Middleware.redirect ~status:st_str location
              | Router.Not_found { view } ->
                let doc, _ = render_full_document ~title:("404 - " ^ app.cfg.title) ~live_script:app.cfg.client_script ~live:true view in
                Middleware.html ~status:"404 Not Found" doc
              | Router.Matched { route; view; _ } ->
                let page_title = Option.value ~default:app.cfg.title route.title in
                let doc, compiled =
                  render_full_document
                    ~title:page_title
                    ?meta:route.meta
                    ~live_script:app.cfg.client_script
                    ~live:true
                    view
                in
                Hashtbl.replace app.visitor_views session.sid
                  { compiled; view_builder = (fun () -> app.cfg.render_for_session session r) };
                if r.meth = "HEAD" then
                  { (Middleware.html doc) with body = "" }
                else
                  Middleware.html doc
            )
        in
        let wrapped = Middleware.pipeline app.cfg.middlewares core_handler in
        wrapped req |> attach_session_cookie
    with exn ->
      Middleware.reject ~status:"500 Internal Server Error" (Printexc.to_string exn)
end
