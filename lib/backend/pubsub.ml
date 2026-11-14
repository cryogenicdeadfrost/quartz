(* In-process PubSub topic bus + Server-Sent Events (SSE) wire formatter.
   Lets background jobs or API calls broadcast live updates to `/__quartz_sse`. *)

type event = {
  id : int;
  topic : string;
  payload : string;
  ts : float;
}

let next_event_id = ref 1
let history : event list ref = ref []
let subscribers : (string, (event -> unit) list) Hashtbl.t = Hashtbl.create 16

let subscribe ~topic (fn : event -> unit) =
  let cur = Option.value ~default:[] (Hashtbl.find_opt subscribers topic) in
  Hashtbl.replace subscribers topic (fn :: cur)

let publish ~topic (payload : string) : event =
  let id = !next_event_id in
  incr next_event_id;
  let ev = { id; topic; payload; ts = Unix.gettimeofday () } in
  history := ev :: List.filteri (fun i _ -> i < 15) !history;
  (match Hashtbl.find_opt subscribers topic with
   | Some subs -> List.iter (fun fn -> fn ev) subs
   | None -> ());
  ev

let recent_events () = !history

(* Standard W3C Server-Sent Events frame (`id: ...\nevent: ...\ndata: ...\n\n`) *)
let format_sse_frame (ev : event) : string =
  Printf.sprintf "id: %d\nevent: %s\ndata: %s\n\n" ev.id ev.topic ev.payload
