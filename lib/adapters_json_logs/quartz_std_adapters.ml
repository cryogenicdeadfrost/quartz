(* Yojson & Structured Logs Adapters (`quartz.std_adapters`) implementing
   `Quartz_core.Io.JSON_CODEC` and `Quartz_core.Io.LOGGER`. *)

open Quartz_core

module Yojson_codec : Io.JSON_CODEC = struct
  let validate (raw : string) : (unit, string) result =
    try
      ignore (Yojson.Safe.from_string raw);
      Ok ()
    with exn -> Error (Printexc.to_string exn)

  let format_compact (raw : string) : (string, string) result =
    try Ok (Yojson.Safe.to_string (Yojson.Safe.from_string raw))
    with exn -> Error (Printexc.to_string exn)

  let format_pretty (raw : string) : (string, string) result =
    try Ok (Yojson.Safe.pretty_to_string (Yojson.Safe.from_string raw))
    with exn -> Error (Printexc.to_string exn)
end

let quartz_src = Logs.Src.create "quartz" ~doc:"Quartz full-stack web runtime"

module Logs_logger : Io.LOGGER = struct
  let info msg = Logs.info ~src:quartz_src (fun m -> m "%s" msg)
  let warn msg = Logs.warn ~src:quartz_src (fun m -> m "%s" msg)
  let error msg = Logs.err ~src:quartz_src (fun m -> m "%s" msg)
end
