(* Deferred Promises, AbortController, Timers, and scoped disposal (ES2025/2026). *)

type 'a state =
  | Pending
  | Fulfilled of 'a
  | Rejected of string

type 'a t = {
  mutable state : 'a state;
  mutable on_fulfill : ('a -> unit) list;
  mutable on_reject : (string -> unit) list;
}

type 'a resolver = {
  promise : 'a t;
  resolve : 'a -> unit;
  reject : string -> unit;
}

let state p = p.state

let is_settled p =
  match p.state with Pending -> false | _ -> true

let with_resolvers () : 'a resolver =
  let p = { state = Pending; on_fulfill = []; on_reject = [] } in
  let resolve v =
    match p.state with
    | Pending ->
      p.state <- Fulfilled v;
      let cbs = List.rev p.on_fulfill in
      p.on_fulfill <- [];
      p.on_reject <- [];
      List.iter (fun f -> f v) cbs
    | _ -> ()
  in
  let reject err =
    match p.state with
    | Pending ->
      p.state <- Rejected err;
      let cbs = List.rev p.on_reject in
      p.on_fulfill <- [];
      p.on_reject <- [];
      List.iter (fun f -> f err) cbs
    | _ -> ()
  in
  { promise = p; resolve; reject }

let make (executor : ('a -> unit) -> (string -> unit) -> unit) : 'a t =
  let r = with_resolvers () in
  (try executor r.resolve r.reject
   with exn -> r.reject (Printexc.to_string exn));
  r.promise

let resolve v = { state = Fulfilled v; on_fulfill = []; on_reject = [] }
let reject err = { state = Rejected err; on_fulfill = []; on_reject = [] }

let try_ (fn : unit -> 'a) : 'a t =
  try resolve (fn ())
  with exn -> reject (Printexc.to_string exn)

let on_settled p ~ok ~err =
  match p.state with
  | Fulfilled v -> ok v
  | Rejected e -> err e
  | Pending ->
    p.on_fulfill <- ok :: p.on_fulfill;
    p.on_reject <- err :: p.on_reject

let bind p (f : 'a -> 'b t) : 'b t =
  let r = with_resolvers () in
  let wire next_p = on_settled next_p ~ok:r.resolve ~err:r.reject in
  on_settled p
    ~ok:(fun v ->
      try wire (f v)
      with exn -> r.reject (Printexc.to_string exn))
    ~err:r.reject;
  r.promise

let then_ = bind

let map p (f : 'a -> 'b) : 'b t =
  bind p (fun v -> resolve (f v))

let catch p (f : string -> 'a t) : 'a t =
  let r = with_resolvers () in
  on_settled p ~ok:r.resolve ~err:(fun e ->
    try on_settled (f e) ~ok:r.resolve ~err:r.reject
    with exn -> r.reject (Printexc.to_string exn));
  r.promise

let finally p (cleanup : unit -> unit) : 'a t =
  let r = with_resolvers () in
  on_settled p
    ~ok:(fun v ->
      (try cleanup () with _ -> ());
      r.resolve v)
    ~err:(fun e ->
      (try cleanup () with _ -> ());
      r.reject e);
  r.promise

let ( let* ) = bind
let ( let+ ) = map

let all (ps : 'a t list) : 'a list t =
  let n = List.length ps in
  if n = 0 then resolve []
  else
    let r = with_resolvers () in
    let results = Array.make n None in
    let remaining = ref n in
    List.iteri
      (fun idx p ->
        on_settled p
          ~ok:(fun v ->
            results.(idx) <- Some v;
            decr remaining;
            if !remaining = 0 then
              let out =
                Array.to_list results
                |> List.map (function Some x -> x | None -> assert false)
              in
              r.resolve out)
          ~err:r.reject)
      ps;
    r.promise

let race (ps : 'a t list) : 'a t =
  let r = with_resolvers () in
  List.iter (fun p -> on_settled p ~ok:r.resolve ~err:r.reject) ps;
  r.promise

let any (ps : 'a t list) : 'a t =
  let n = List.length ps in
  if n = 0 then reject "AggregateError: all promises were rejected"
  else
    let r = with_resolvers () in
    let errs = Array.make n "" in
    let remaining = ref n in
    List.iteri
      (fun idx p ->
        on_settled p ~ok:r.resolve ~err:(fun e ->
          errs.(idx) <- e;
          decr remaining;
          if !remaining = 0 then
            r.reject ("AggregateError: " ^ String.concat "; " (Array.to_list errs))))
      ps;
    r.promise

type 'a settled =
  | Ok_settled of 'a
  | Err_settled of string

let all_settled (ps : 'a t list) : 'a settled list t =
  let n = List.length ps in
  if n = 0 then resolve []
  else
    let r = with_resolvers () in
    let results = Array.make n (Err_settled "") in
    let remaining = ref n in
    let finish () =
      decr remaining;
      if !remaining = 0 then r.resolve (Array.to_list results)
    in
    List.iteri
      (fun idx p ->
        on_settled p
          ~ok:(fun v ->
            results.(idx) <- Ok_settled v;
            finish ())
          ~err:(fun e ->
            results.(idx) <- Err_settled e;
            finish ()))
      ps;
    r.promise

module Timers = struct
  type id = int

  type entry = {
    id : id;
    mutable fire_at : float;
    interval_ms : float option;
    fn : unit -> unit;
    mutable cancelled : bool;
  }

  let next_id = ref 1
  let entries : entry list ref = ref []
  let microtasks : (unit -> unit) Queue.t = Queue.create ()

  let clear_all () =
    entries := [];
    Queue.clear microtasks

  let queue_microtask fn = Queue.add fn microtasks

  let flush_microtasks () =
    while not (Queue.is_empty microtasks) do
      let fn = Queue.take microtasks in
      fn ()
    done

  let set_timeout ?(clock = Io.Clock_hook.now_s) ~ms fn : id =
    let id = !next_id in
    incr next_id;
    let now = clock () in
    let e = { id; fire_at = now +. (float_of_int (max 0 ms) /. 1000.0); interval_ms = None; fn; cancelled = false } in
    entries := e :: !entries;
    id

  let set_interval ?(clock = Io.Clock_hook.now_s) ~ms fn : id =
    let id = !next_id in
    incr next_id;
    let step = float_of_int (max 1 ms) /. 1000.0 in
    let e = { id; fire_at = clock () +. step; interval_ms = Some step; fn; cancelled = false } in
    entries := e :: !entries;
    id

  let clear_timer id =
    List.iter (fun e -> if e.id = id then e.cancelled <- true) !entries;
    entries := List.filter (fun e -> not e.cancelled) !entries

  let tick ?(clock = Io.Clock_hook.now_s) () : int =
    flush_microtasks ();
    let now = clock () in
    let fired = ref 0 in
    let due = List.filter (fun e -> not e.cancelled && e.fire_at <= now) (List.rev !entries) in
    List.iter
      (fun e ->
        if not e.cancelled then (
          incr fired;
          (match e.interval_ms with
           | Some step -> e.fire_at <- now +. step
           | None -> e.cancelled <- true);
          e.fn ()
        ))
      due;
    entries := List.filter (fun e -> not e.cancelled) !entries;
    flush_microtasks ();
    !fired

  let debounce ?(clock = Io.Clock_hook.now_s) ~ms (fn : 'a -> unit) : 'a -> unit =
    let pending : id option ref = ref None in
    fun x ->
      Option.iter clear_timer !pending;
      pending := Some (set_timeout ~clock ~ms (fun () -> pending := None; fn x))

  let throttle ?(clock = Io.Clock_hook.now_s) ~ms (fn : 'a -> unit) : 'a -> unit =
    let last_run = ref neg_infinity in
    let step = float_of_int (max 0 ms) /. 1000.0 in
    fun x ->
      let now = clock () in
      if now -. !last_run >= step then (
        last_run := now;
        fn x
      )
end

module Abort = struct
  type signal = {
    mutable aborted : bool;
    mutable reason : string option;
    mutable listeners : (string -> unit) list;
  }

  type controller = {
    signal : signal;
  }

  let create_controller () : controller =
    { signal = { aborted = false; reason = None; listeners = [] } }

  let signal c = c.signal
  let is_aborted s = s.aborted
  let reason s = s.reason

  let abort ?(reason = "AbortError") (c : controller) =
    if not c.signal.aborted then (
      c.signal.aborted <- true;
      c.signal.reason <- Some reason;
      let cbs = List.rev c.signal.listeners in
      c.signal.listeners <- [];
      List.iter (fun f -> f reason) cbs
    )

  let on_abort (s : signal) (fn : string -> unit) =
    if s.aborted then fn (Option.value ~default:"AbortError" s.reason)
    else s.listeners <- fn :: s.listeners

  let any (signals : signal list) : signal =
    let ctrl = create_controller () in
    List.iter (fun s -> on_abort s (fun r -> abort ~reason:r ctrl)) signals;
    ctrl.signal

  let timeout ?(clock = Io.Clock_hook.now_s) ~ms () : signal =
    let ctrl = create_controller () in
    ignore (Timers.set_timeout ~clock ~ms (fun () -> abort ~reason:"TimeoutError" ctrl));
    ctrl.signal
end

module Scope = struct
  type t = {
    mutable disposed : bool;
    mutable cleanups : (unit -> unit) list;
  }

  let create () = { disposed = false; cleanups = [] }

  let defer s fn =
    if s.disposed then fn ()
    else s.cleanups <- fn :: s.cleanups

  let dispose s =
    if not s.disposed then (
      s.disposed <- true;
      let fns = s.cleanups in
      s.cleanups <- [];
      List.iter (fun f -> try f () with _ -> ()) fns
    )

  let with_scope fn =
    let s = create () in
    Fun.protect ~finally:(fun () -> dispose s) (fun () -> fn s)
end
