open Quartz

type deployment = {
  service : string;
  region : string;
  rps : string;
  healthy : bool;
}

let saas_app () =
  let deploys, set_deploys =
    use_state
      [
        { service = "auth-gateway"; region = "eu-central-1"; rps = "14,200"; healthy = true };
        { service = "order-router"; region = "us-east-1"; rps = "89,450"; healthy = true };
        { service = "webhook-worker"; region = "ap-Tokyo-1"; rps = "3,110"; healthy = false };
      ]
  in
  let req_multiplier, set_mult = use_state 1 in
  let est_cost = use_memo (fun () -> !~req_multiplier * 49) in
  let modal_open = use_signal false in
  let email_field = Form.create_field ~initial:"alex@acme.dev" Form.email in
  let service_field = Form.create_field ~initial:"billing-api" (Form.min_len 3) in

  let home_page _ =
    div
      ~style:Style.[ flex_col; gap 24; p 24; max_w 1100; mx_auto ]
      [
        div
          ~style:Style.[ flex_row; justify_between; items_center ]
          [
            div
              ~style:Style.[ flex_col; gap 4 ]
              [
                h1 ~style:Style.[ text_3xl; bold; m 0 ] [ text "Quartz Cloud Control Plane" ];
                p ~style:Style.[ text_sm; color Palette.slate_400; m 0 ]
                  [ text "Zero-npm OCaml LiveView dashboard with reactive Signals & pure-SVG telemetry." ];
              ];
            btn ~variant:Primary ~on_click:(fun () -> modal_open <~ true) "+ New Service";
          ];
        div
          ~style:Style.[ grid_cols 3; gap 16 ]
          [
            stat_card ~label:"Global Throughput" ~value:"106,760 rps" ~delta:"+14.2%" ();
            stat_card ~label:"P99 Latency" ~value:"1.84 ms" ~delta:"-0.3 ms" ();
            rx (fun () ->
              stat_card
                ~label:"Estimated Monthly Spend"
                ~value:(Printf.sprintf "$%d/mo" !~est_cost)
                ~delta:(Printf.sprintf "%dx Cluster" !~req_multiplier)
                ());
          ];
        div
          ~style:Style.[ grid_cols 2; gap 16 ]
          [
            card
              ~title:"Traffic Sparkline (Last 10m)"
              ~subtitle:"Live request volume across edge pops"
              [ sparkline ~width:420 ~height:80 [ 42.0; 48.0; 45.0; 61.0; 58.0; 74.0; 89.0; 95.0; 91.0; 106.0 ] ];
            card
              ~title:"Interactive Cluster Scaler"
              ~subtitle:"Reactive Signal.computed updates cost in real time"
              [
                div
                  ~style:Style.[ flex_row; items_center; gap 12; py 12 ]
                  [
                    btn ~variant:Neutral
                      ~on_click:(fun () -> set_mult (max 1 (!~req_multiplier - 1)))
                      "- Scale Down";
                    rx (fun () ->
                      badge ~variant:Primary (Printf.sprintf "%d Replicas Active" !~req_multiplier));
                    btn ~variant:Success
                      ~on_click:(fun () -> set_mult (!~req_multiplier + 1))
                      "+ Scale Up";
                  ];
              ];
          ];
        card
          ~title:"Active Microservices"
          [
            rx (fun () ->
              data_table
                ~columns:
                  [
                    ("Service", fun d -> strong [ text d.service ]);
                    ("Region", fun d -> code [ text d.region ]);
                    ("Throughput", fun d -> text (d.rps ^ " req/s"));
                    ( "Status",
                      fun d ->
                        if d.healthy then badge ~variant:Success "Healthy"
                        else badge ~variant:Danger "Degraded" );
                  ]
                ~rows:!~deploys);
          ];
        modal
          ~open_sig:modal_open
          ~title:"Deploy New Service"
          [
            Form.view_input ~label:"Service Name" service_field;
            Form.view_input ~label:"On-Call Email" email_field;
            div
              ~style:Style.[ flex_row; justify_end; gap 8; mt 12 ]
              [
                btn ~variant:Success
                  ~on_click:(fun () ->
                    match Form.validate service_field, Form.validate email_field with
                    | Ok srv, Ok _ ->
                      set_deploys
                        (!~deploys
                        @ [ { service = srv; region = "us-west-2"; rps = "1,000"; healthy = true } ]);
                      modal_open <~ false
                    | _ -> ())
                  "Launch Service";
              ];
          ];
      ]
  in

  let pricing_page _ =
    div
      ~style:Style.[ flex_col; gap 16; p 24; max_w 800; mx_auto ]
      [
        h2 ~style:Style.[ text_2xl; bold ] [ text "Simple Usage Pricing" ];
        alert ~variant:Success ~title:"Free Tier Included"
          "First 1,000,000 WebSocket LiveView patches per month are completely free.";
        card ~title:"Regional Breakdown"
          [ bar_chart ~width:400 ~height:120 [ ("US", 89.0); ("EU", 54.0); ("APAC", 31.0) ] ];
      ]
  in

  div
    ~style:Style.[ min_h 100; bg Palette.slate_950; color Palette.slate_100 ]
    [
      navbar
        ~brand:"⚡ Quartz"
        ~right:(badge ~variant:Success "OCaml 5 Ready")
        [ ("/", "Dashboard"); ("/pricing", "Pricing") ];
      Router.router
        [
          Router.route "/" home_page;
          Router.route "/pricing" pricing_page;
        ];
    ]

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "serve" :: port_s :: _ ->
    let port = Option.value ~default:8080 (int_of_string_opt port_s) in
    run_browser ~port ~title:"Quartz Cloud Dashboard" saas_app
  | _ :: "serve" :: _ ->
    run_browser ~port:8080 ~title:"Quartz Cloud Dashboard" saas_app
  | _ ->
    Printf.printf ">>> [1/2] Exporting multi-page static HTML/CSS site to ./dist_site <<<\n";
    export_site ~out_dir:"dist_site" ~title:"Quartz Cloud Dashboard"
      [
        ("index.html", "/", saas_app);
        ("pricing.html", "/pricing", saas_app);
      ];
    Printf.printf "Exported ./dist_site/index.html and ./dist_site/pricing.html\n\n";
    Printf.printf ">>> [2/2] Rendering the exact same component tree via Flexbox Terminal UI <<<\n";
    Router.navigate "/";
    run_terminal ~width:76 ~height:18 saas_app
