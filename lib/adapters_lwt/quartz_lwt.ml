(* Lwt Async I/O Adapter (`quartz.lwt`) implementing `Quartz_core.Io.IO`.
   Enables MirageOS, Lwt servers, and Cohttp apps to share pure Quartz core code
   without hardwiring Lwt into `quartz.core`. *)

open Quartz_core

module Lwt_io : Io.IO with type 'a t = 'a Lwt.t = struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let map = Lwt.map
end

module Lwt_ops = Io.Ops (Lwt_io)

module Storage_of_sync (S : Io.SYNC_STORAGE) :
  Io.STORAGE with type 'a io = 'a Lwt.t =
  Io.Async_storage_of_sync (S) (Lwt_io)

let run_sync (promise : 'a Lwt.t) : 'a =
  match Lwt.state promise with
  | Lwt.Return v -> v
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep -> failwith "Quartz_lwt.run_sync: promise still pending"
