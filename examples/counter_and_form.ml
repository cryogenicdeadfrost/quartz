(* Look how close this is to React + TypeScript, except with zero npm/webpack
   and 100% native OCaml type safety. *)

open Quartz

let app () =
  let count, set_count = use_state 0 in
  let doubled = use_memo (fun () -> !~count * 2) in

  div ~style:Style.[ flex_col; gap 16; p 24; bg Palette.slate_900; rounded_xl ] [
    h1 ~style:Style.[ text_2xl; bold; color Palette.sky_400 ] [
      text "Quartz Reactive Counter"
    ];
    rx (fun () ->
      p ~style:Style.[ text_lg; color Palette.slate_200 ] [
        textf "Count: %d (Doubled: %d)" !~count !~doubled
      ]);
    div ~style:Style.[ flex_row; gap 10 ] [
      btn ~variant:Primary ~on_click:(fun () -> set_count (!~count + 1)) "+ Increment";
      btn ~variant:Neutral ~on_click:(fun () -> set_count (!~count - 1)) "- Decrement";
    ]
  ]

let () =
  Printf.printf "--- HTML Output ---\n%s\n\n--- Terminal TUI Output ---\n"
    (render_html (app ()));
  run_terminal ~width:56 ~height:10 app
