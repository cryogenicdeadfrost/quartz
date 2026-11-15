(* TypeScript `.d.ts` -> Typed OCaml Plug Module Generator (`Quartz.Import_dts`).
   Converts TypeScript component props interfaces and exported function types
   into ready-to-compile OCaml wrappers backed by `Npm_plug.component` and
   `Npm_plug.call_typed`, eliminating manual per-package FFI maintenance. *)

type ts_field = {
  fname : string;
  ts_type : string;
  optional : bool;
}

type ts_interface = {
  iname : string;
  fields : ts_field list;
}

type ts_func = {
  fn_name : string;
  params : ts_field list;
  ret_type : string;
}

let strip_ts_comments (src : string) : string =
  let len = String.length src in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && src.[!i] = '/' && src.[!i + 1] = '/' then (
      while !i < len && src.[!i] <> '\n' do incr i done
    ) else if !i + 1 < len && src.[!i] = '/' && src.[!i + 1] = '*' then (
      i := !i + 2;
      while !i + 1 < len && not (src.[!i] = '*' && src.[!i + 1] = '/') do incr i done;
      i := min len (!i + 2)
    ) else (
      Buffer.add_char buf src.[!i];
      incr i
    )
  done;
  Buffer.contents buf

let ocaml_ident (s : string) : string =
  let lower =
    let buf = Buffer.create (String.length s + 4) in
    String.iteri
      (fun idx c ->
        if c >= 'A' && c <= 'Z' then (
          if idx > 0 then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
        ) else if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' then
          Buffer.add_char buf c
        else Buffer.add_char buf '_')
      s;
    Buffer.contents buf
  in
  match lower with
  | "type" | "val" | "let" | "in" | "open" | "module" | "end" | "match" | "with"
  | "fun" | "function" | "to" | "for" | "do" | "done" | "begin" | "method" -> lower ^ "_"
  | _ -> lower

let rec ocaml_type_of_ts (raw : string) : string * string * string =
  (* returns (ocaml_type, json_encoder_fn, json_decoder_fn) *)
  let t = String.trim raw in
  let lt = String.lowercase_ascii t in
  if lt = "string" then ("string", "Json.string", "Json.Decode.string")
  else if lt = "number" then ("float", "Json.float", "Json.Decode.float")
  else if lt = "boolean" then ("bool", "Json.bool", "Json.Decode.bool")
  else if String.length t > 2 && String.sub t (String.length t - 2) 2 = "[]" then
    let inner = String.sub t 0 (String.length t - 2) in
    let o_in, enc_in, dec_in = ocaml_type_of_ts inner in
    ( o_in ^ " list",
      Printf.sprintf "(fun xs -> Json.array (List.map %s xs))" enc_in,
      Printf.sprintf "(Json.Decode.list %s)" dec_in )
  else ("Json.t", "Fun.id", "Json.Decode.value")

let parse_param_list (raw : string) : ts_field list =
  String.split_on_char ',' raw
  |> List.filter_map (fun part ->
         let p = String.trim part in
         if p = "" then None
         else
           match String.index_opt p ':' with
           | None -> None
           | Some ci ->
             let raw_name = String.trim (String.sub p 0 ci) in
             let optional = String.length raw_name > 0 && raw_name.[String.length raw_name - 1] = '?' in
             let clean_name =
               if optional then String.sub raw_name 0 (String.length raw_name - 1) else raw_name
             in
             let ts_type =
               String.trim (String.sub p (ci + 1) (String.length p - ci - 1))
             in
             Some { fname = clean_name; ts_type; optional })

let parse_dts (src : string) : ts_interface list * ts_func list =
  let clean = strip_ts_comments src in
  let lines = String.split_on_char '\n' clean in
  let ifaces = ref [] in
  let funcs = ref [] in
  let cur_iface : (string * ts_field list) option ref = ref None in
  List.iter
    (fun raw_line ->
      let line = String.trim raw_line in
      match !cur_iface with
      | Some (iname, fields) ->
        if String.contains line '}' then (
          ifaces := { iname; fields = List.rev fields } :: !ifaces;
          cur_iface := None
        ) else if line <> "" then (
          let stripped =
            let n = String.length line in
            if n > 0 && (line.[n - 1] = ';' || line.[n - 1] = ',') then String.sub line 0 (n - 1) else line
          in
          match parse_param_list stripped with
          | f :: _ -> cur_iface := Some (iname, f :: fields)
          | [] -> ()
        )
      | None ->
        let has prefix =
          let lp = String.length prefix in
          String.length line >= lp && String.sub line 0 lp = prefix
        in
        if has "export interface " || has "interface " then (
          let after =
            if has "export interface " then String.sub line 17 (String.length line - 17)
            else String.sub line 10 (String.length line - 10)
          in
          let iname =
            match String.index_opt after '{' with
            | Some idx -> String.trim (String.sub after 0 idx)
            | None -> String.trim after
          in
          if String.contains line '{' && String.contains line '}' then (
            match String.index_opt line '{', String.rindex_opt line '}' with
            | Some l, Some r when r > l ->
              let inner = String.sub line (l + 1) (r - l - 1) in
              let fs =
                String.split_on_char ';' inner
                |> List.concat_map (fun s -> parse_param_list s)
              in
              ifaces := { iname; fields = fs } :: !ifaces
            | _ -> ()
          ) else cur_iface := Some (iname, [])
        ) else if has "export declare function " || has "declare function " || has "export function " then (
          let after_fn =
            match String.index_opt line '(' with
            | None -> None
            | Some lp ->
              let prefix_part = String.trim (String.sub line 0 lp) in
              let toks = String.split_on_char ' ' prefix_part |> List.filter (( <> ) "") in
              let fn_name = match List.rev toks with h :: _ -> h | [] -> "call" in
              (match String.rindex_opt line ')' with
               | Some rp when rp >= lp ->
                 let params_s = String.sub line (lp + 1) (rp - lp - 1) in
                 let ret_raw = String.sub line (rp + 1) (String.length line - rp - 1) in
                 let ret_type =
                   match String.index_opt ret_raw ':' with
                   | Some ci ->
                     let r = String.trim (String.sub ret_raw (ci + 1) (String.length ret_raw - ci - 1)) in
                     let n = String.length r in
                     if n > 0 && r.[n - 1] = ';' then String.trim (String.sub r 0 (n - 1)) else r
                   | None -> "any"
                 in
                 Some { fn_name; params = parse_param_list params_s; ret_type }
               | _ -> None)
          in
          match after_fn with Some f -> funcs := f :: !funcs | None -> ()
        ))
    lines;
  (List.rev !ifaces, List.rev !funcs)

let generate_ocaml_plug ~(pkg : string) ?(version = "latest") (dts_src : string) : string =
  let ifaces, funcs = parse_dts dts_src in
  let buf = Buffer.create 1024 in
  Printf.bprintf buf
    "(* Auto-generated by `quartz gen-plug` from TypeScript .d.ts for %S *)\nopen Quartz\n\n" pkg;
  List.iter
    (fun (iface : ts_interface) ->
      let tname = ocaml_ident iface.iname in
      Printf.bprintf buf "type %s = {\n" tname;
      List.iter
        (fun f ->
          let ot, _, _ = ocaml_type_of_ts f.ts_type in
          let full_t = if f.optional then ot ^ " option" else ot in
          Printf.bprintf buf "  %s : %s;\n" (ocaml_ident f.fname) full_t)
        iface.fields;
      Printf.bprintf buf "}\n\n";
      Printf.bprintf buf "let encode_%s (v : %s) : (string * Json.t) list =\n  let acc = ref [] in\n" tname tname;
      List.iter
        (fun f ->
          let _, enc, _ = ocaml_type_of_ts f.ts_type in
          let o_field = ocaml_ident f.fname in
          if f.optional then
            Printf.bprintf buf "  (match v.%s with Some x -> acc := (%S, %s x) :: !acc | None -> ());\n"
              o_field f.fname enc
          else
            Printf.bprintf buf "  acc := (%S, %s v.%s) :: !acc;\n" f.fname enc o_field)
        iface.fields;
      Printf.bprintf buf "  List.rev !acc\n\n")
    ifaces;
  List.iter
    (fun (fn : ts_func) ->
      let oname = ocaml_ident fn.fn_name in
      let is_component =
        match fn.params with
        | [ p ] ->
          List.exists (fun i -> i.iname = p.ts_type) ifaces
          && (String.length fn.fn_name > 0 && fn.fn_name.[0] >= 'A' && fn.fn_name.[0] <= 'Z')
        | _ -> false
      in
      if is_component then (
        let p = List.hd fn.params in
        let props_id = ocaml_ident p.ts_type in
        Printf.bprintf buf
          "let %s ?(framework = `React) (props : %s) : Dom.node =\n  Npm_plug.component ~pkg:%S ~version:%S ~export_name:%S ~framework ~props:(encode_%s props) ()\n\n"
          oname props_id pkg version fn.fn_name props_id
      ) else (
        let _, _, ret_dec = ocaml_type_of_ts fn.ret_type in
        let params_sig =
          String.concat " "
            (List.map
               (fun p ->
                 let ot, _, _ = ocaml_type_of_ts p.ts_type in
                 Printf.sprintf "~%s:(%s : %s)" (ocaml_ident p.fname) (ocaml_ident p.fname) ot)
               fn.params)
        in
        let args_list =
          if fn.params = [] then "[]"
          else
            "[ "
            ^ String.concat "; "
                (List.map
                   (fun p ->
                     let _, enc, _ = ocaml_type_of_ts p.ts_type in
                     Printf.sprintf "%s %s" enc (ocaml_ident p.fname))
                   fn.params)
            ^ " ]"
        in
        Printf.bprintf buf
          "let %s (module T : Io.PLUG_TRANSPORT) %s () =\n  Npm_plug.call_typed (module T) ~pkg:%S ~fn:%S ~encode:Json.array ~decode:%s %s\n\n"
          oname params_sig pkg fn.fn_name ret_dec args_list
      ))
    funcs;
  Buffer.contents buf
