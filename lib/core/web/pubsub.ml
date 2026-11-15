(* In-process PubSub topic bus + Server-Sent Events (SSE) wire formatter.
   Zero Unix dependency: clock injected via Io.Clock_hook. *)

type event = {
  id : int;
  topic : string;
  payload : string;
  ts : float;
}

let next_event_id = ref 1
let next_sub_id = ref 1
let history : event list ref = ref []
let subscribers : (string, (int * (event -> unit)) list) Hashtbl.t = Hashtbl.create 16

let subscribe_id ~topic (fn : event -> unit) : int =
  let id = !next_sub_id in
  incr next_sub_id;
  let cur = Option.value ~default:[] (Hashtbl.find_opt subscribers topic) in
  Hashtbl.replace subscribers topic ((id, fn) :: cur);
  id

let subscribe ~topic (fn : event -> unit) =
  ignore (subscribe_id ~topic fn)

let unsubscribe id =
  Hashtbl.iter
    (fun topic lst ->
      Hashtbl.replace subscribers topic (List.filter (fun (sid, _) -> sid <> id) lst))
    subscribers

let publish ?(clock = Io.Clock_hook.now_s) ~topic (payload : string) : event =
  let id = !next_event_id in
  incr next_event_id;
  let ev = { id; topic; payload; ts = clock () } in
  history := ev :: List.filteri (fun i _ -> i < 31) !history;
  (match Hashtbl.find_opt subscribers topic with
   | Some subs -> List.iter (fun (_, fn) -> fn ev) (List.rev subs)
   | None -> ());
  ev

let recent_events () = !history

let format_sse_frame (ev : event) : string =
  Printf.sprintf "id: %d\nevent: %s\ndata: %s\n\n" ev.id ev.topic ev.payload

module Bus : Io.PUBSUB = struct
  let publish ~topic msg = ignore (publish ~topic msg)
  let subscribe ~topic fn = subscribe_id ~topic (fun ev -> fn ev.payload)
  let unsubscribe = unsubscribe
end
