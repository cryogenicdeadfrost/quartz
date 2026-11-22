(* .env / .env.local config loader with strict server vs public-client separation. *)

open Quartz_core

let store : (string, string) Hashtbl.t = Hashtbl.create 32

let read_file_opt path =
  if Sys.file_exists path && not (Sys.is_directory path) then
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  else None

let load ?(paths = [ ".env"; ".env.local" ]) () : int =
  let loaded = ref 0 in
  List.iter
    (fun p ->
      match read_file_opt p with
      | None -> ()
      | Some raw ->
        List.iter
          (fun (k, v) ->
            Hashtbl.replace store k v;
            incr loaded)
          (Es_std.Env_parser.parse_dotenv raw))
    paths;
  !loaded

let load_string (src : string) : int =
  let pairs = Es_std.Env_parser.parse_dotenv src in
  List.iter (fun (k, v) -> Hashtbl.replace store k v) pairs;
  List.length pairs

let set k v = Hashtbl.replace store k v
let clear () = Hashtbl.clear store

let get (k : string) : string option =
  match Sys.getenv_opt k with
  | Some v -> Some v
  | None -> Hashtbl.find_opt store k

let get_default k default =
  match get k with Some v when v <> "" -> v | _ -> default

let require k : (string, string) result =
  match get k with
  | Some v when String.trim v <> "" -> Ok v
  | _ -> Error ("Missing required env var: " ^ k)

let int ?(default = 0) k : int =
  match get k with
  | Some v -> Option.value ~default (int_of_string_opt (String.trim v))
  | None -> default

let bool ?(default = false) k : bool =
  match get k with
  | Some v ->
    (match String.lowercase_ascii (String.trim v) with
     | "1" | "true" | "yes" | "on" -> true
     | "0" | "false" | "no" | "off" -> false
     | _ -> default)
  | None -> default

let all_loaded () =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) store []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let public_vars ?(prefixes = [ "PUBLIC_"; "QUARTZ_PUBLIC_"; "VITE_" ]) () =
  Es_std.Env_parser.filter_public ~prefixes (all_loaded ())

let emit_public_script ?prefixes () =
  let fields =
    List.map (fun (k, v) -> (k, Json.string v)) (public_vars ?prefixes ())
  in
  "window.__QUARTZ_ENV__ = " ^ Json.to_string (Json.obj fields) ^ ";"
