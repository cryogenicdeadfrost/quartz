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
  let ls = String.length s and lsub = String.length sub in
  let rec loop i =
    if i + lsub > ls then false
    else if String.sub s i lsub = sub then true
    else loop (i + 1)
  in
  lsub = 0 || loop 0

let test_npm_plug_and_browser_runtime () =
  let node_react =
    Npm_plug.react_component
      ~pkg:"lucide-react"
      ~version:"0.460.0"
      ~export_name:"Rocket"
      ~stylesheets:[ "https://example.com/plugin.css" ]
      ~props:[ ("size", Json.int 24); ("strokeWidth", Json.float 1.75) ]
      ~fallback:[ Dom.span [ Dom.text "Icon Loading..." ] ]
      ()
  in
  let c = Dom.compile_node node_react in
  assert_true "Npm_plug emits data-q-plug=1" (contains_sub ~sub:"data-q-plug=\"1\"" c.html);
  assert_true "Npm_plug emits data-q-pkg" (contains_sub ~sub:"data-q-pkg=\"lucide-react\"" c.html);
  assert_true "Npm_plug emits framework=react" (contains_sub ~sub:"data-q-framework=\"react\"" c.html);
  assert_true "Npm_plug registered pkg metadata"
    (List.exists (fun (p, _) -> p = "lucide-react") (Npm_plug.list_registered_plugs ()));
  let imap = Dom.compile_node (Npm_plug.import_map_node [ ("three", "https://esm.sh/three@0.170.0") ]) in
  assert_true "Npm_plug importmap script emitted" (contains_sub ~sub:"type=\"importmap\"" imap.html);
  List.iter
    (fun sym ->
      assert_true ("Browser_runtime exposes " ^ sym)
        (contains_sub ~sub:sym Browser_runtime.client_runtime_js))
    [
      "hydratePlugs";
      "https://esm.sh";
      "react-dom@18/client";
      "devtoolsFormatters";
      "hydrateIntl";
      "Intl.DateTimeFormat";
      "Intl.PluralRules";
      "hydrateHeadlessInteractions";
      "data-q-combobox";
      "data-q-tooltip-wrap";
    ]

let test_devtools_registry_and_inspector () =
  Debug_registry.register_variant ~type_name:"route_state" ~constructors:[ "Loading"; "Ready"; "Failed" ];
  Debug_registry.register_record ~type_name:"user_row" ~fields:[ "id"; "email"; "role" ];
  let table_js = Debug_registry.emit_devtools_table_script () in
  assert_true "Debug_registry emits window.__QUARTZ_DEBUG_TYPES__"
    (contains_sub ~sub:"window.__QUARTZ_DEBUG_TYPES__" table_js);
  assert_true "Debug_registry emits variant constructors"
    (contains_sub ~sub:"\"Loading\"" table_js);
  let tagged = Debug_registry.tag_value ~type_name:"route_state" (Json.obj [ ("tag", Json.int 1) ]) in
  assert_true "Debug_registry tags value"
    (contains_sub ~sub:"\"__quartzType\":\"route_state\"" (Json.to_string tagged));
  let insp_html =
    Debug_registry.render_inspector_html
      ~title:"Studio"
      ~engine:"quartz-v4.2"
      ~uptime_s:1.25
      ~requests_handled:7
      ~routes:[ Router.route ~title:"Home" "/" (fun _ -> Dom.empty) ]
      ~api_endpoints:[ "/api/health" ]
      ~recent_events:[]
      ~access_logs:[ "{\"path\":\"/\"}" ]
      ~registered_plugs:(Npm_plug.list_registered_plugs ())
  in
  assert_true "Inspector HTML renders title & plug list"
    (contains_sub ~sub:"Studio -- Runtime Inspector" insp_html
     && contains_sub ~sub:"lucide-react@0.460.0" insp_html)

let test_cdn_images_intl_and_h2 () =
  let v_url = Dom.cdn_image_url ~provider:`Vercel ~width:800 "/hero.png" in
  let cf_url = Dom.cdn_image_url ~provider:`Cloudflare ~width:640 "/hero.png" in
  let ip_url = Dom.cdn_image_url ~provider:(`Imgproxy "https://img.example.com") ~width:480 "/hero.png" in
  assert_true "Vercel CDN URL format" (contains_sub ~sub:"/_vercel/image?url=%2Fhero.png&w=800" v_url);
  assert_true "Cloudflare CDN URL format" (contains_sub ~sub:"/cdn-cgi/image/width=640" cf_url);
  assert_true "Imgproxy CDN URL format" (contains_sub ~sub:"https://img.example.com/insecure/rs:fit:480:0" ip_url);
  let resp_img = Dom.responsive_img ~provider:`Cloudflare ~src:"/banner.jpg" ~alt:"Banner" ~width:1200 ~height:630 () in
  let img_diag = Dom.diagnose resp_img in
  assert_eq_int "Responsive image has 0 CLS/a11y warnings" ~expected:0 ~actual:(List.length img_diag.a11y_warnings);
  let d_html = (Dom.compile_node (Dom.intl_date ~epoch_s:1763164800L ())).html in
  assert_true "Dom.intl_date emits data-q-intl-date" (contains_sub ~sub:"data-q-intl-date=\"1763164800\"" d_html);
  assert_true "Http.is_h2_preface detects PRI preface"
    (Http.is_h2_preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\nextra")

let test_optional_opam_adapters () =
  (* 1. Lwt Adapter *)
  let open Quartz_lwt in
  let module Async_mem = Storage_of_sync (Store.Wal_store (Unix_adapters.Unix_clock)) in
  let p =
    let open Lwt_ops in
    let* () = Async_mem.set "lwt_key" "hello_lwt" in
    let+ v = Async_mem.get "lwt_key" in
    v
  in
  assert_eq_str "Quartz_lwt Async storage round-trip" ~expected:"hello_lwt" ~actual:(Option.get (run_sync p));

  (* 2. Uutf + Uucp Unicode Adapter *)
  let module U : Io.UNICODE = Quartz_intl_uutf.Quartz_uutf.Uutf_unicode in
  assert_true "Uutf detects valid multi-byte UTF-8" (U.is_valid_utf8 "Caf\xc3\xa9 Zurich");
  assert_true "Uutf rejects invalid UTF-8" (not (U.is_valid_utf8 "\xff\xfe\xfd"));
  assert_eq_int "Uutf counts Unicode codepoints accurately" ~expected:4 ~actual:(U.char_count "Caf\xc3\xa9");
  assert_eq_str "Uucp Unicode casefold" ~expected:"caf\xc3\xa9" ~actual:(U.casefold "CAF\xc3\x89");
  assert_eq_str "Uucp slugify" ~expected:"quartz-web-v4" ~actual:(U.slugify "  Quartz Web v4! ");

  (* 3. Re PCRE Regex Adapter *)
  let module Rx : Io.REGEX = Quartz_re.Re_regex in
  assert_true "Re_regex matches email pattern" (Rx.matches ~pattern:"^[a-z0-9.]+@[a-z]+\\.[a-z]{2,}$" "dev@ocaml.org");
  assert_eq_int "Re_regex finds all tokens" ~expected:4 ~actual:(List.length (Rx.find_all ~pattern:"\\d+" "v4.2 build 108 on port 8080"));
  assert_eq_str "Re_regex replace_all" ~expected:"hello-world" ~actual:(Rx.replace_all ~pattern:"\\s+" ~with_str:"-" "hello   world");
  assert_true "Re_form pcre_validator passes" (Result.is_ok (Quartz_re.pcre_validator ~pattern:"^v[0-9]+\\.[0-9]+$" "v4.2" ));

  (* 4. Yojson + Logs Adapters *)
  let module Jc : Io.JSON_CODEC = Quartz_std_adapters.Yojson_codec in
  assert_true "Yojson_codec validates JSON" (Result.is_ok (Jc.validate "{\"ok\":true,\"items\":[1,2]}"));
  assert_true "Yojson_codec rejects broken JSON" (Result.is_error (Jc.validate "{\"bad\":"));
  let log_lines = ref [] in
  let custom_reporter =
    {
      Logs.report =
        (fun _src _level ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
              Format.kasprintf (fun s -> log_lines := s :: !log_lines; over (); k ()) fmt));
    }
  in
  Logs.set_reporter custom_reporter;
  Logs.set_level (Some Logs.Info);
  Quartz_std_adapters.Logs_logger.info "Quartz structured adapter check";
  assert_true "Logs_logger dispatches via Logs reporter"
    (List.exists (fun s -> contains_sub ~sub:"Quartz structured adapter check" s) !log_lines)

let test_npm_server_plug_and_dts () =
  (* 1. Real server-side Node/Bun JS worker execution when runtime is present *)
  (match Npm_server_plug.detect_js_runtime () with
   | None -> failwith "Expected node runtime to verify Npm_server_plug live IPC"
   | Some _ ->
     let res1 = Npm_server_plug.eval_js ~expr_fn:"(args) => ({ sum: args[0] + args[1], upper: args[2].toUpperCase() })" ~args_json:"[19, 23, \"quartz\"]" in
     (match res1 with
      | Error e -> failwith ("Npm_server_plug eval_js failed: " ^ e)
      | Ok raw ->
        assert_true "Npm_server_plug returned computed JSON from real JS runtime"
          (contains_sub ~sub:"\"sum\":42" raw && contains_sub ~sub:"\"QUARTZ\"" raw));
     let module Math_plug = struct
       let multiply_and_tag a b =
         Npm_plug.call_typed
           (module Npm_server_plug.Worker_transport)
           ~pkg:"__builtin_eval"
           ~fn:"(x) => ({ prod: x.a * x.b, engine: 'node-worker' })"
           ~encode:(fun (va, vb) -> Json.obj [ ("a", Json.int va); ("b", Json.int vb) ])
           ~decode:Json.Decode.(field "prod" int)
           (a, b)
     end in
     assert_eq_int "Typed OCaml -> JS Worker -> Typed OCaml int" ~expected:56
       ~actual:(Result.get_ok (Math_plug.multiply_and_tag 7 8));
     Npm_server_plug.stop_worker ());

  (* 2. TypeScript .d.ts -> OCaml Plug Generator *)
  let dts_sample =
    {|
export interface StripePayProps {
  clientSecret: string;
  amount: number;
  darkMode?: boolean;
}
export declare function StripeCheckout(props: StripePayProps): any;
export declare function formatAmount(cents: number, currency: string): string;
|}
  in
  let gen_ml = Import_dts.generate_ocaml_plug ~pkg:"@stripe/react-stripe-js" ~version:"2.8.0" dts_sample in
  assert_true "Import_dts generates props record" (contains_sub ~sub:"type stripe_pay_props =" gen_ml);
  assert_true "Import_dts generates optional field" (contains_sub ~sub:"dark_mode : bool option;" gen_ml);
  assert_true "Import_dts generates React Npm_plug component wrapper"
    (contains_sub ~sub:"Npm_plug.component ~pkg:\"@stripe/react-stripe-js\"" gen_ml);
  assert_true "Import_dts generates typed server call wrapper"
    (contains_sub ~sub:"Npm_plug.call_typed (module T)" gen_ml)

let test_multicloud_deploy_and_html_headless () =
  let tmp = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "quartz_cloud_test_%d" (Unix.getpid ())) in
  let v_native = Filename.concat tmp "vc_native" in
  ignore (Deploy.emit_vercel_output ~out_dir:v_native ~mode:`Native ());
  let ic = open_in_bin (Filename.concat v_native "functions/index.func/.vc-config.json") in
  let vc_native_cfg = really_input_string ic (in_channel_length ic) in
  close_in ic;
  assert_true "Vercel native matches Build Output API v3 custom bootstrap spec"
    (contains_sub ~sub:"\"custom\"" vc_native_cfg
     && contains_sub ~sub:"\"bootstrap\"" vc_native_cfg
     && contains_sub ~sub:"\"Lambda\"" vc_native_cfg);

  let v_node = Filename.concat tmp "vc_node" in
  ignore (Deploy.emit_vercel_output ~out_dir:v_node ~mode:`Nodejs ());
  assert_true "Vercel Nodejs output generated"
    (Sys.file_exists (Filename.concat v_node "functions/index.func/index.js"));

  let deno_dir = Filename.concat tmp "deno" in
  ignore (Deploy.emit_deno_deploy ~out_dir:deno_dir ());
  assert_true "Deno Deploy deno.json + main.ts created"
    (Sys.file_exists (Filename.concat deno_dir "deno.json")
     && Sys.file_exists (Filename.concat deno_dir "main.ts"));

  let bun_dir = Filename.concat tmp "bun" in
  ignore (Deploy.emit_bun_serve ~out_dir:bun_dir ());
  assert_true "Bun serve server.ts + bunfig.toml created"
    (Sys.file_exists (Filename.concat bun_dir "bunfig.toml")
     && Sys.file_exists (Filename.concat bun_dir "server.ts"));

  let netlify_dir = Filename.concat tmp "netlify" in
  ignore (Deploy.emit_netlify_edge ~out_dir:netlify_dir ());
  assert_true "Netlify edge config created"
    (Sys.file_exists (Filename.concat netlify_dir "netlify.toml")
     && Sys.file_exists (Filename.concat netlify_dir "netlify/edge-functions/quartz.js"));

  let sam_dir = Filename.concat tmp "sam" in
  ignore (Deploy.emit_aws_sam_lambda ~out_dir:sam_dir ());
  assert_true "AWS SAM template created" (Sys.file_exists (Filename.concat sam_dir "template.yaml"));

  let caddy_file = Deploy.emit_caddy_https ~out_dir:tmp () in
  assert_true "Caddyfile created" (Sys.file_exists caddy_file);

  let rr_dir = Filename.concat tmp "rr" in
  ignore (Deploy.emit_railway_render_cloudrun ~out_dir:rr_dir ());
  assert_true "Railway + Render configs created"
    (Sys.file_exists (Filename.concat rr_dir "railway.toml")
     && Sys.file_exists (Filename.concat rr_dir "render.yaml"));

  (* Import_html maps <dialog>, <details> and <img> dimensions *)
  let html_snippet = "<dialog id=\"d1\" open><p>Hi</p></dialog><img src=\"/x.png\" alt=\"X\"/>" in
  let ml_code = Import_html.convert_html_to_ocaml html_snippet in
  assert_true "Import_html maps <dialog> to Headless.dialog" (contains_sub ~sub:"Headless.dialog" ml_code);
  assert_true "Import_html assigns default width/height on <img>" (contains_sub ~sub:"~width:320 ~height:180" ml_code)

let () =
  test_npm_plug_and_browser_runtime ();
  test_devtools_registry_and_inspector ();
  test_cdn_images_intl_and_h2 ();
  test_optional_opam_adapters ();
  test_npm_server_plug_and_dts ();
  test_multicloud_deploy_and_html_headless ();
  Printf.printf "[OK] Quartz Adapters, Plugs & Known Gaps Suite: %d assertions passed!\n" !passed
