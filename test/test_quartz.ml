open Quartz

let check msg cond =
  if not cond then (
    Printf.eprintf "[FAIL] %s\n%!" msg;
    exit 1
  )

let has_sub haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found && String.sub haystack i nlen = needle then found := true
    done;
    !found

let test_signals () =
  let open Signal in
  let count, set_count = create 2 in
  let doubled = computed (fun () -> !~count * 2) in
  let quad = computed (fun () -> !~doubled * 2) in
  check "initial computed 2*2=4" (read doubled = 4);
  check "initial chained computed 4*2=8" (read quad = 8);

  let runs = ref 0 in
  let last_seen = ref 0 in
  let eff =
    watch (fun () ->
      incr runs;
      last_seen := read quad)
  in
  check "effect runs once on init" (!runs = 1 && !last_seen = 8);

  set_count 5;
  check "effect reacts to set_count 5 -> 20" (!runs = 2 && !last_seen = 20);

  (* batch multiple updates so effect only fires once *)
  batch (fun () ->
    set_count 10;
    set_count 15;
    set_count 20);
  check "batch coalesced 3 sets into 1 run" (!runs = 3 && !last_seen = 80);

  dispose_effect eff;
  set_count 99;
  check "disposed effect stops firing" (!runs = 3)

let test_dom_and_styles () =
  let open Dom in
  let count = Signal.make 7 in
  let clicked = ref false in
  let view =
    div
      ~style:Style.[ flex_col; gap 12; p 16; bg Palette.slate_900; hover [ bg Palette.slate_800 ]; md [ flex_row ] ]
      [
        h1 ~style:Style.[ text_2xl; bold; color Palette.sky_400 ] [ text "Quartz <App>" ];
        rx_text (fun () -> Printf.sprintf "Count is %d" (Signal.get count));
        button ~label:"Hit me" ~on_click:(fun () -> clicked := true) [];
      ]
  in
  let compiled = compile_node view in
  check "HTML escapes <App>" (has_sub compiled.html "Quartz &lt;App&gt;");
  check "Dynamic rx_text rendered" (has_sub compiled.html "Count is 7");
  check "Hover CSS generated" (has_sub compiled.css ":hover{background-color:#1e293b}");
  check "Responsive @media generated" (has_sub compiled.css "@media(min-width:768px)");
  check "Click handler registered" (Hashtbl.length compiled.handlers = 1);

  (* invoke registered click handler *)
  Hashtbl.iter
    (fun _ h ->
      match h with On_click fn -> fn () | _ -> ())
    compiled.handlers;
  check "Registered click callback executed" !clicked

let () =
  test_signals ();
  test_dom_and_styles ();
  Printf.printf "[OK] Quartz Iteration 1 (Signal graph, Atomic CSS & DOM compiler) passed.\n%!"
