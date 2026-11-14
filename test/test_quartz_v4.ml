(* Comprehensive Test Suite for Quartz v4.0 ("One Core, Many Targets") *)

open Quartz

let passed = ref 0

let assert_true msg cond =
  if not cond then failwith ("ASSERT FAILED: " ^ msg) else incr passed

let assert_eq_str msg ~expected ~actual =
  if expected <> actual then
    failwith (Printf.sprintf "ASSERT FAILED (%s): expected %S, got %S" msg expected actual)
  else incr passed

let assert_eq_int msg ~expected ~actual =
  if expected <> actual then
    failwith (Printf.sprintf "ASSERT FAILED (%s): expected %d, got %d" msg expected actual)
  else incr passed

let contains_sub ~sub s =
  let sl = String.length sub and tl = String.length s in
  if sl = 0 then true
  else
    let rec loop i =
      if i + sl > tl then false
      else if String.sub s i sl = sub then true
      else loop (i + 1)
    in
    loop 0

let test_json_and_decode () =
  let raw = {|{"user":{"name":"Ada\n\"Lovelace\"","age":36,"ok":true,"tags":["math","code"]},"emoji":"\uD83D\uDE80"}|} in
  match Json.parse raw with
  | Error e -> failwith ("JSON parse failed: " ^ e)
  | Ok v ->
    let rt = Json.to_string v in
    assert_true "JSON roundtrip parses back" (Result.is_ok (Json.parse rt));
    let dec =
      Json.Decode.(
        field "user"
          (both (field "name" string)
             (both (field "age" int) (field "tags" (list string)))))
    in
    (match Json.Decode.decode_string dec raw with
     | Error e -> failwith ("Decode failed: " ^ e)
     | Ok (name, (age, tags)) ->
       assert_eq_str "decoded name" ~expected:"Ada\n\"Lovelace\"" ~actual:name;
       assert_eq_int "decoded age" ~expected:36 ~actual:age;
       assert_eq_int "decoded tags len" ~expected:2 ~actual:(List.length tags));
    (* Structured JSONPath error reporting *)
    let bad = {|{"user":{"tags":["ok", 42]}}|} in
    let tags_dec = Json.Decode.(field "user" (field "tags" (list string))) in
    (match Json.Decode.decode_string tags_dec bad with
     | Ok _ -> failwith "Expected decode error"
     | Error err -> assert_true "JSONPath error mentions $.user.tags.[1]" (contains_sub ~sub:"tags.[1]" err))

let test_crypto_vectors () =
  (* FIPS 180-4 SHA-256("abc") *)
  let h_abc = Crypto_util.sha256_hex "abc" in
  assert_eq_str "FIPS SHA-256('abc')"
    ~expected:"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    ~actual:h_abc;
  (* RFC 4231 HMAC-SHA256 Test Case 1 *)
  let key1 = String.make 20 '\x0b' in
  let mac1 = Crypto_util.hmac_sha256_hex ~secret:key1 "Hi There" in
  assert_eq_str "RFC 4231 Test Case 1"
    ~expected:"b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
    ~actual:mac1;
  (* RFC 4231 HMAC-SHA256 Test Case 2 *)
  let mac2 = Crypto_util.hmac_sha256_hex ~secret:"Jefe" "what do ya want for nothing?" in
  assert_eq_str "RFC 4231 Test Case 2"
    ~expected:"5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
    ~actual:mac2;
  (* RFC 6455 Sec-WebSocket-Accept exact vector *)
  let ws_accept = Websocket.accept_key ~sec_websocket_key:"dGhlIHNhbXBsZSBub25jZQ==" in
  assert_eq_str "RFC 6455 WebSocket accept_key"
    ~expected:"s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    ~actual:ws_accept;
  (* Base64 & Base64url roundtrip *)
  let enc = Crypto_util.base64_encode "foobar" in
  assert_eq_str "RFC 4648 base64('foobar')" ~expected:"Zm9vYmFy" ~actual:enc;
  assert_eq_str "base64 decode" ~expected:"foobar"
    ~actual:(Option.get (Crypto_util.base64_decode enc))

let test_cookies_and_sessions () =
  let secret = "super-secret-signing-key-2026" in
  let signed = Cookie.sign ~secret ~value:"sid_42" in
  assert_eq_str "verify valid cookie" ~expected:"sid_42"
    ~actual:(Option.get (Cookie.verify ~secret signed));
  let tampered = signed ^ "x" in
  assert_true "tampered cookie rejected" (Option.is_none (Cookie.verify ~secret tampered));
  let module Mem = Store.Wal_store (Unix_adapters.Unix_clock) in
  let module Sess = Cookie.Session.Make (Unix_adapters.Unix_clock) (Mem) in
  let mgr = Sess.create_manager ~ttl_s:300 ~secret () in
  let s1, is_new1 = Sess.load_or_create mgr ~cookie_header:"" in
  assert_true "first request creates new session" is_new1;
  Sess.set_val mgr "user" "alice" s1;
  let hdr_val =
    let sc = Sess.set_cookie_header mgr s1 in
    match String.split_on_char ';' sc with kv :: _ -> kv | [] -> ""
  in
  let s1_reloaded, is_new_re = Sess.load_or_create mgr ~cookie_header:hdr_val in
  assert_true "subsequent request loads existing session" (not is_new_re);
  assert_eq_str "session preserves per-visitor state" ~expected:"alice"
    ~actual:(Option.get (Sess.get_val "user" s1_reloaded));
  let s2, is_new2 = Sess.load_or_create mgr ~cookie_header:"" in
  assert_true "visitor 2 gets distinct session" (is_new2 && s2.sid <> s1.sid);
  assert_true "visitor 2 cannot read visitor 1 state" (Option.is_none (Sess.get_val "user" s2))

let test_i18n_and_calendar () =
  assert_true "valid UTF-8" (I18n.utf8_valid "Hello \xE2\x82\xAC");
  assert_true "invalid UTF-8 rejected" (not (I18n.utf8_valid "\xFF\xFE"));
  assert_eq_int "utf8_length counts codepoints" ~expected:5 ~actual:(I18n.utf8_length "ab\xE2\x82\xACcd");
  assert_true "Arabic zero plural" (I18n.plural_category ~locale:"ar" 0 = I18n.Zero);
  assert_true "Russian few plural (2)" (I18n.plural_category ~locale:"ru" 2 = I18n.Few);
  let cat =
    [
      ("en", [ ("items.one", "{{count}} item"); ("items.other", "{{count}} items") ]);
      ("fr", [ ("items.one", "{{count}} element") ]);
    ]
  in
  assert_eq_str "i18n en plural 1" ~expected:"1 item" ~actual:(I18n.t ~catalog:cat ~locale:"en" ~count:1 "items");
  assert_eq_str "i18n en plural 5" ~expected:"5 items" ~actual:(I18n.t ~catalog:cat ~locale:"en" ~count:5 "items");
  (* UTC RFC 7231 epoch check: 784111777 = Sun, 06 Nov 1994 08:49:37 GMT *)
  assert_eq_str "RFC 7231 HTTP date"
    ~expected:"Sun, 06 Nov 1994 08:49:37 GMT"
    ~actual:(I18n.to_http_date 784111777L)

let test_oklch_apca_spring_and_3d () =
  let scale = Contrast.generate_scale (`Hex "#38bdf8") in
  assert_eq_int "OKLCH scale has 11 stops" ~expected:11 ~actual:(List.length scale);
  let lc = Contrast.apca_lc ~fg:(`Hex "#ffffff") ~bg:(`Hex "#020617") in
  assert_true "white on slate-950 passes APCA >= 90" (Float.abs lc >= 90.0);
  let curve, dur_ms = Style.spring_solve ~stiffness:220.0 ~damping:18.0 ~mass:1.0 () in
  assert_true "spring curve starts with linear(" (contains_sub ~sub:"linear(" curve);
  assert_true "spring duration realistic" (dur_ms >= 120);
  let scene = Scene3d.create [ Scene3d.mesh ~name:"cube" () ] in
  let j_str = Scene3d.to_json_string scene in
  assert_true "Scene3d serializes cube" (contains_sub ~sub:"\"cube\"" j_str)

let test_dom_diff_headless_and_islands () =
  let render_cnt n =
    Dom.div ~id:"box" ~attrs:[ Dom.attr "data-step" (string_of_int n) ] [
      Dom.p [ Dom.text ("Count=" ^ string_of_int n) ];
      Dom.island ~name:"counter" ~props:[ ("n", Json.int n) ] (Dom.text "SSR");
      Headless.accordion_item
        ~summary_style:[]
        ~panel_style:[]
        ~summary_kids:[ Dom.text "FAQ" ]
        [ Dom.text "Answer" ];
    ]
  in
  let c1 = Dom.compile_node (render_cnt 1) in
  let c2 = Dom.compile_node (render_cnt 2) in
  let patches = Dom.diff c1.snapshot c2.snapshot in
  let pjson = Json.to_string (Dom.patches_to_json patches) in
  assert_true "DOM diff contains set_text for Count=2" (contains_sub ~sub:"Count=2" pjson);
  assert_true "DOM diff contains set_attr data-step=2" (contains_sub ~sub:"\"data-step\"" pjson);
  assert_true "Island marker emitted" (contains_sub ~sub:"data-q-island=\"counter\"" c1.html);
  assert_true "Headless details rendered" (contains_sub ~sub:"<details" c1.html)

let test_gadt_router_and_honeypot () =
  let make_spec () = Router.Path.(s "org" @@ str @@ s "repo" @@ int @@ bool nil) in
  let url = Router.Path.href (make_spec ()) "ocaml" 42 true in
  assert_eq_str "GADT href formats safely" ~expected:"/org/ocaml/repo/42/true" ~actual:url;
  let route_def =
    Router.gadt_route (make_spec ()) (fun org id flag ->
      Dom.text (Printf.sprintf "%s:%d:%b" org id flag))
  in
  let redirects = [ { Router.from_path = "/old"; to_path = "/org/ocaml/repo/42/true"; status_code = 301 } ] in
  (match Router.resolve ~redirects [ route_def ] "/old" with
   | Router.Redirected { status_code = 301; location } ->
     assert_eq_str "301 redirect location" ~expected:"/org/ocaml/repo/42/true" ~actual:location
   | _ -> failwith "Expected 301 Redirected");
  (match Router.resolve ~redirects [ route_def ] "/missing" with
   | Router.Not_found _ -> incr passed
   | _ -> failwith "Expected 404 Not_found");
  (match Router.resolve ~redirects [ route_def ] url with
   | Router.Matched { view; _ } ->
     let c = Dom.compile_node view in
     assert_eq_str "GADT handler output" ~expected:"ocaml:42:true" ~actual:c.html
   | _ -> failwith "Expected Matched GADT route");
  assert_true "Honeypot rejects filled trap"
    (Result.is_error (Form.check_honeypot [ ("_hp_company_url", "http://spam.example") ]));
  assert_true "Honeypot allows blank trap"
    (Result.is_ok (Form.check_honeypot [ ("_hp_company_url", "") ]))

let test_middleware_rpc_ws_multipart_http () =
  let spec, box =
    Middleware.Endpoint.define
      ~name:"sumPair"
      ~meth:"POST"
      ~path:"/api/rpc/sum"
      ~decode_req:Json.Decode.(both (field "a" int) (field "b" int))
      ~encode_res:(fun s -> Json.obj [ ("sum", Json.int s) ])
      (fun _req (a, b) -> Ok (a + b))
  in
  (match Middleware.Endpoint.invoke_local spec box (fun (a, b) -> Json.obj [ ("a", Json.int a); ("b", Json.int b) ]) (7, 35) with
   | Ok s -> assert_true "Typed RPC returns sum 42" (contains_sub ~sub:"42" s)
   | Error e -> failwith ("RPC failed: " ^ e));
  (* WebSocket masked frame decode -> server frame encode + Rooms *)
  let mask = [| 0x37; 0xfa; 0x21; 0x3d |] in
  let plain = "Hello WS!" in
  let plen = String.length plain in
  let raw_ws = Bytes.create (6 + plen) in
  Bytes.set raw_ws 0 '\x81';
  Bytes.set raw_ws 1 (Char.chr (0x80 lor plen));
  for i = 0 to 3 do Bytes.set raw_ws (2 + i) (Char.chr mask.(i)) done;
  for i = 0 to plen - 1 do
    Bytes.set raw_ws (6 + i) (Char.chr (Char.code plain.[i] lxor mask.(i mod 4)))
  done;
  (match Websocket.decode_frame (Bytes.unsafe_to_string raw_ws) with
   | Websocket.Frame (fr, "") ->
     assert_eq_str "WS decoded masked payload" ~expected:"Hello WS!" ~actual:fr.payload;
     assert_true "WS opcode Text" (fr.opcode = Websocket.Text)
   | _ -> failwith "WS decode_frame failed");
  let got = ref "" in
  Websocket.Rooms.join ~room:"chat" ~conn_id:101 (fun wire -> got := wire);
  let n_sent = Websocket.Rooms.broadcast ~room:"chat" "ping" in
  assert_eq_int "WS room broadcast count" ~expected:1 ~actual:n_sent;
  assert_true "WS room wire non-empty" (String.length !got > 2);
  Websocket.Rooms.leave_all ~conn_id:101;
  (* Multipart upload + PNG magic byte sniffing *)
  let png_magic = "\x89PNG\r\n\x1a\nfake-png-bytes" in
  let boundary = "qbound123" in
  let mbody =
    Printf.sprintf
      "--%s\r\nContent-Disposition: form-data; name=\"avatar\"; filename=\"icon.png\"\r\nContent-Type: application/octet-stream\r\n\r\n%s\r\n--%s--\r\n"
      boundary png_magic boundary
  in
  (match Multipart.parse ~content_type:("multipart/form-data; boundary=" ^ boundary) mbody with
   | Ok [ part ] ->
     assert_eq_str "Multipart sniffed MIME overrides octet-stream" ~expected:"image/png" ~actual:part.sniffed_mime;
     assert_eq_str "Multipart field name" ~expected:"avatar" ~actual:part.field_name
   | _ -> failwith "Multipart parse failed");
  (* Chunked HTTP/1.1 + Range header *)
  let chunked_raw =
    "POST /upload HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n World\r\n0\r\n\r\n"
  in
  (match Http.try_parse_request chunked_raw with
   | Http.Complete (req, "") ->
     assert_eq_str "HTTP chunked decoded body" ~expected:"Hello World" ~actual:req.body
   | _ -> failwith "HTTP chunked parse failed");
  (match Http.parse_range ~total_len:10 "bytes=2-5" with
   | Some (2, 5) -> incr passed
   | _ -> failwith "HTTP parse_range failed")

let test_wal_store_migrations_and_jobs () =
  let tmp_wal = Filename.temp_file "quartz_wal_test_" ".log" in
  let store = Unix_adapters.File_storage.open_wal ~wal_path:tmp_wal in
  Unix_adapters.File_storage.set store "k1" "v1";
  Unix_adapters.File_storage.set store "k2" "v2";
  Unix_adapters.File_storage.delete store "k2";
  ignore (Unix_adapters.File_storage.compact store);
  let reopened = Unix_adapters.File_storage.open_wal ~wal_path:tmp_wal in
  assert_eq_str "File WAL persists across reopen" ~expected:"v1"
    ~actual:(Option.get (Unix_adapters.File_storage.get reopened "k1"));
  assert_true "File WAL deleted key gone after reopen"
    (Option.is_none (Unix_adapters.File_storage.get reopened "k2"));
  (try Sys.remove tmp_wal with _ -> ());
  (* Migrations + Jobs *)
  let module Mem = Store.Wal_store (Unix_adapters.Unix_clock) in
  let module Mig = Store.Migrations (Mem) in
  let migs =
    [
      { Mig.version = 1; name = "init"; up = (fun () -> Mem.set "schema" "v1"; Ok ()) };
      { Mig.version = 2; name = "seed"; up = (fun () -> Mem.set "seeded" "1"; Ok ()) };
    ]
  in
  assert_eq_int "2 migrations ran first time" ~expected:2 ~actual:(Result.get_ok (Mig.run migs));
  assert_eq_int "0 migrations ran idempotently" ~expected:0 ~actual:(Result.get_ok (Mig.run migs));
  Jobs.clear ();
  let fake_now = ref 100.0 in
  let _j = Jobs.enqueue ~clock:(fun () -> !fake_now) ~max_retries:2 ~name:"failer" "payload" in
  ignore (Jobs.process_queue ~clock:(fun () -> !fake_now) [ ("failer", fun _ -> Error "boom") ]);
  fake_now := 110.0;
  ignore (Jobs.process_queue ~clock:(fun () -> !fake_now) [ ("failer", fun _ -> Error "boom2") ]);
  assert_eq_int "Job enters Dead_letter after max_retries" ~expected:1 ~actual:(List.length (Jobs.dead_letter_jobs ()))

let test_universal_targets_edge_mirage_vercel_deploy_import () =
  let module Mem = Store.Wal_store (Unix_adapters.Unix_clock) in
  let module E = Quartz_edge.Edge_adapter.Make (Unix_adapters.Unix_clock) (Mem) (Pubsub.Bus) in
  let module M = Quartz_mirage.Mirage_adapter.Make (Unix_adapters.Unix_clock) (Quartz_mirage.Mirage_adapter.Empty_kv_ro) (Mem) (Pubsub.Bus) in
  let cfg_routes = [ Router.route ~title:"Home" "/" (fun _ -> Dom.h1 [ Dom.text "Universal Core" ]) ] in
  let app_cfg ~app_name =
    {
      E.Core_app.title = app_name;
      session_secret = "edge-mirage-secret";
      session_ttl_s = 3600;
      secure_cookies = false;
      client_script = "";
      routes = cfg_routes;
      redirects = [];
      middlewares = [ Middleware.security_headers () ];
      rpcs = [];
      get_apis = [ ("/api/ping", fun _ -> Middleware.ok {|{"pong":true}|}) ];
      post_apis = [];
      static_assets = [];
      render_for_session = (fun _ _ -> Dom.h1 [ Dom.text "Universal Core" ]);
    }
  in
  let e_app = E.Core_app.create (app_cfg ~app_name:"EdgeApp") in
  let fetch_res =
    E.handle_fetch_json e_app {|{"method":"GET","url":"https://example.com/api/ping","headers":{},"body":""}|}
  in
  assert_true "Edge WinterCG adapter serves /api/ping" (contains_sub ~sub:"pong" fetch_res);
  let m_cfg : M.Core_app.config =
    {
      title = "MirageApp";
      session_secret = "edge-mirage-secret";
      session_ttl_s = 3600;
      secure_cookies = false;
      client_script = "";
      routes = cfg_routes;
      redirects = [];
      middlewares = [];
      rpcs = [];
      get_apis = [];
      post_apis = [];
      static_assets = [];
      render_for_session = (fun _ _ -> Dom.h1 [ Dom.text "Universal Core" ]);
    }
  in
  let m_app = M.create m_cfg in
  (match M.handle_raw_http m_app "GET / HTTP/1.1\r\nHost: unikernel\r\n\r\n" with
   | Http.Complete _, Some wire ->
     assert_true "Mirage raw HTTP returns 200 OK" (contains_sub ~sub:"200 OK" wire);
     assert_true "Mirage raw HTTP renders Universal Core" (contains_sub ~sub:"Universal Core" wire)
   | _ -> failwith "Mirage handle_raw_http failed");
  (* Vercel Lambda event parsing & response *)
  let ev_json = {|{"rawPath":"/api/ping","requestContext":{"http":{"method":"POST"}},"headers":{"content-type":"application/json"},"body":"aGVsbG8=","isBase64Encoded":true}|} in
  let vreq = Quartz_vercel.Vercel_runtime.req_of_lambda_event ev_json in
  assert_eq_str "Vercel b64 decoded body" ~expected:"hello" ~actual:vreq.body;
  assert_eq_str "Vercel method POST" ~expected:"POST" ~actual:vreq.meth;
  let vres = Quartz_vercel.Vercel_runtime.lambda_response_of_res (Middleware.ok {|{"ok":1}|}) in
  assert_true "Vercel Lambda JSON has statusCode 200" (contains_sub ~sub:"\"statusCode\":200" vres);
  (* HTML + Tailwind -> OCaml Codegen *)
  let ocaml_code =
    Import_html.convert_html_to_ocaml
      {|<div class="flex p-4 bg-slate-900 rounded-xl"><button class="text-sm font-bold text-white">Save</button></div>|}
  in
  assert_true "import-html generates Dom.div" (contains_sub ~sub:"Dom.div" ocaml_code);
  assert_true "import-html converts flex & p 16" (contains_sub ~sub:"Style.[ flex; p 16" ocaml_code);
  assert_true "import-html converts button" (contains_sub ~sub:"Dom.button" ocaml_code)

let test_app_live_session_deploy_and_middlewares () =
  let module Mem = Store.Wal_store (Unix_adapters.Unix_clock) in
  let module A = App.Make (Unix_adapters.Unix_clock) (Mem) (Pubsub.Bus) in
  let per_visitor_views session =
    let cur =
      match Option.bind (A.Sessions.get_val "clicks" session) int_of_string_opt with
      | Some n -> n
      | None -> 0
    in
    Dom.div [
      Dom.button
        ~on_click:(fun () ->
          let prev =
            match Option.bind (A.Sessions.get_val "clicks" session) int_of_string_opt with
            | Some n -> n
            | None -> 0
          in
          Mem.set ("s:" ^ session.sid) (string_of_int (prev + 1));
          session.data <- ("clicks", string_of_int (prev + 1)) :: List.remove_assoc "clicks" session.data)
        [ Dom.text "Bump" ];
      Dom.p ~id:"count" [ Dom.text (Printf.sprintf "Clicks:%d" cur) ];
    ]
  in
  let routes = [ Router.route ~title:"Home" "/" (fun _ -> Dom.div [ Dom.text "Home" ]) ] in
  let redirects = [ { Router.from_path = "/legacy"; to_path = "/"; status_code = 301 } ] in
  let app =
    A.create
      {
        title = "FullApp";
        session_secret = "app-secret-2026";
        session_ttl_s = 3600;
        secure_cookies = false;
        client_script = Browser_runtime.client_runtime_js;
        routes;
        redirects;
        middlewares = [ Middleware.body_limit ~max_bytes:128 (); Middleware.etag_cache () ];
        rpcs = [];
        get_apis = [];
        post_apis = [];
        static_assets = [];
        render_for_session = (fun sess _ -> per_visitor_views sess);
      }
  in
  let r_home =
    A.handle app
      { Middleware.meth = "GET"; path = "/"; query = []; headers = []; body = ""; client_ip = "127.0.0.1" }
  in
  assert_eq_int "App / returns 200" ~expected:200 ~actual:(Middleware.status_code_of_res r_home);
  assert_true "App / emits Set-Cookie" (List.mem_assoc "Set-Cookie" r_home.headers);
  let r_301 =
    A.handle app
      { Middleware.meth = "GET"; path = "/legacy"; query = []; headers = []; body = ""; client_ip = "127.0.0.1" }
  in
  assert_eq_int "App /legacy returns 301" ~expected:301 ~actual:(Middleware.status_code_of_res r_301);
  let r_404 =
    A.handle app
      { Middleware.meth = "GET"; path = "/nope"; query = []; headers = []; body = ""; client_ip = "127.0.0.1" }
  in
  assert_eq_int "App /nope returns 404" ~expected:404 ~actual:(Middleware.status_code_of_res r_404);
  let r_413 =
    A.handle app
      { Middleware.meth = "POST"; path = "/__quartz_event"; query = []; headers = []; body = String.make 256 'a'; client_ip = "127.0.0.1" }
  in
  assert_eq_int "App body_limit rejects >128 bytes with 413" ~expected:413 ~actual:(Middleware.status_code_of_res r_413);
  (* Conditional ETag 304 check *)
  let etag = Option.get (List.assoc_opt "ETag" r_home.headers) in
  let r_304 =
    A.handle app
      { Middleware.meth = "GET"; path = "/"; query = []; headers = [ ("If-None-Match", etag) ]; body = ""; client_ip = "127.0.0.1" }
  in
  assert_eq_int "App ETag cache returns 304 Not Modified" ~expected:304 ~actual:(Middleware.status_code_of_res r_304);
  (* LiveView click event returns DOM diff patch updating Clicks:0 -> Clicks:1 *)
  let cookie_kv =
    match List.assoc_opt "Set-Cookie" r_home.headers with
    | Some sc -> (match String.split_on_char ';' sc with kv :: _ -> kv | [] -> "")
    | None -> ""
  in
  let r_evt =
    A.handle app
      {
        Middleware.meth = "POST";
        path = "/__quartz_event";
        query = [];
        headers = [ ("Cookie", cookie_kv) ];
        body = "id=0&val=";
        client_ip = "127.0.0.1";
      }
  in
  assert_eq_int "App /__quartz_event returns 200" ~expected:200 ~actual:(Middleware.status_code_of_res r_evt);
  assert_true "App /__quartz_event emits patches or html JSON" (contains_sub ~sub:"patches" r_evt.body || contains_sub ~sub:"html" r_evt.body);
  (* Deploy generators emit real files *)
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "quartz_deploy_%d" (Unix.getpid ())) in
  let pages_yml = Deploy.emit_github_pages ~out_dir:tmp_dir () in
  assert_true "Deploy GitHub Pages file exists" (Sys.file_exists pages_yml);
  let cf_dir = Filename.concat tmp_dir "cf" in
  ignore (Deploy.emit_cloudflare_worker ~out_dir:cf_dir ());
  assert_true "Deploy Cloudflare wrangler.toml exists" (Sys.file_exists (Filename.concat cf_dir "wrangler.toml"));
  let fly_dir = Filename.concat tmp_dir "fly" in
  ignore (Deploy.emit_docker_and_fly ~out_dir:fly_dir ());
  assert_true "Deploy Dockerfile exists" (Sys.file_exists (Filename.concat fly_dir "Dockerfile"));
  let mirage_dir = Filename.concat tmp_dir "mirage" in
  ignore (Deploy.emit_mirage ~out_dir:mirage_dir ());
  assert_true "Deploy Mirage unikernel.ml exists" (Sys.file_exists (Filename.concat mirage_dir "unikernel.ml"));
  let llms = Deploy.generate_llms_txt ~title:"Quartz" ~base_url:"https://quartz.dev" routes in
  assert_true "llms.txt lists routes" (contains_sub ~sub:"https://quartz.dev/" llms);
  let ci_yml = Deploy.emit_github_ci ~out_dir:tmp_dir () in
  assert_true "Deploy CI workflow exists" (Sys.file_exists ci_yml)

let test_v4_2_motion_webgpu_listbox_loader_and_fuzz () =
  (* Style: @view-transition, @starting-style, CSS deduplication *)
  let st_cls = Style.starting_style ~cls:"panel" [ Style.opacity 0.0 ] in
  let _, _, rules =
    Style.compile_styles ~cls_prefix:"vt"
      [ Style.view_transition_navigation; Style.view_transition_name "hero"; st_cls ]
  in
  let joined = String.concat "\n" rules in
  assert_true "@view-transition emitted" (contains_sub ~sub:"@view-transition" joined);
  assert_true "@starting-style emitted" (contains_sub ~sub:"@starting-style" joined);
  assert_eq_int "CSS dedup removes duplicates" ~expected:2
    ~actual:(List.length (Style.deduplicate_css [ "a"; "b"; "a"; ""; "b" ]));
  (* Scene3d WebGPU + WGSL shader serialization *)
  let gpu_scene =
    Scene3d.create ~renderer:`WebGPU
      [
        Scene3d.mesh ~name:"wgsl-quad"
          ~material:
            (Scene3d.Wgsl_shader
               {
                 vertex_wgsl = "@vertex fn vs() -> @builtin(position) vec4<f32> { return vec4<f32>(0.0); }";
                 fragment_wgsl = "@fragment fn fs() -> @location(0) vec4<f32> { return vec4<f32>(1.0); }";
                 uniforms = [ ("u_time", 1.0) ];
               })
          ();
      ]
  in
  let gj = Scene3d.to_json_string gpu_scene in
  assert_true "Scene3d serializes webgpu renderer" (contains_sub ~sub:"\"webgpu\"" gj);
  assert_true "Scene3d serializes wgsl_shader" (contains_sub ~sub:"\"wgsl_shader\"" gj);
  (* Headless listbox + FLIP marker support in Browser runtime *)
  let lb =
    Headless.listbox ~id:"lb1" ~multiselectable:true
      [ ("Alpha", true, None); ("Beta", false, None) ]
  in
  let lb_c = Dom.compile_node lb in
  assert_true "Headless listbox ARIA attributes" (contains_sub ~sub:"aria-multiselectable=\"true\"" lb_c.html);
  assert_true "Browser runtime includes FLIP + WebGPU"
    (contains_sub ~sub:"data-q-flip" Browser_runtime.client_runtime_js
     && contains_sub ~sub:"navigator.gpu" Browser_runtime.client_runtime_js);
  (* Route loader + Request ID middleware *)
  let loaded_id = ref "" in
  let loader_routes =
    [
      Router.route ~loader:(fun ps -> loaded_id := Option.value ~default:"" (Router.param "id" ps))
        "/item/:id" (fun _ -> Dom.text "item");
    ]
  in
  (match Router.resolve loader_routes "/item/99" with
   | Router.Matched _ -> assert_eq_str "Router loader invoked before render" ~expected:"99" ~actual:!loaded_id
   | _ -> failwith "Expected Matched loader route");
  let mw_h = Middleware.pipeline [ Middleware.request_id () ] (fun _ -> Middleware.ok "{}") in
  let r_id =
    mw_h { Middleware.meth = "GET"; path = "/"; query = []; headers = []; body = ""; client_ip = "127.0.0.1" }
  in
  assert_true "Middleware request_id injects X-Request-Id" (List.mem_assoc "X-Request-Id" r_id.headers);
  (* Adversarial / Fuzz safety across Http, Json, Multipart, Cookie *)
  let fuzz_inputs =
    [
      ""; "\r\n"; "\x00\xff\xfe"; "GET\r\n\r\n"; "POST / HTTP/1.1\r\nContent-Length: -5\r\n\r\n";
      "POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\nZZZZ\r\n";
      String.make 20000 'A'; "{\"a\":[[[[["; "\"unterminated"; "...."; "a.b.c";
    ]
  in
  List.iter
    (fun inp ->
      ignore (Http.try_parse_request inp);
      ignore (Json.parse inp);
      ignore (Multipart.parse ~content_type:"multipart/form-data; boundary=abc" inp);
      assert_true "Cookie.verify never crashes on fuzz input"
        (Option.is_none (Cookie.verify ~secret:"fuzz-secret" inp)))
    fuzz_inputs

let () =
  test_json_and_decode ();
  test_crypto_vectors ();
  test_cookies_and_sessions ();
  test_i18n_and_calendar ();
  test_oklch_apca_spring_and_3d ();
  test_dom_diff_headless_and_islands ();
  test_gadt_router_and_honeypot ();
  test_middleware_rpc_ws_multipart_http ();
  test_wal_store_migrations_and_jobs ();
  test_universal_targets_edge_mirage_vercel_deploy_import ();
  test_app_live_session_deploy_and_middlewares ();
  test_v4_2_motion_webgpu_listbox_loader_and_fuzz ();
  Printf.printf "[OK] Quartz v4.0-v4.2 Complete Suite: all %d assertions across 12 test groups passed!\n" !passed
