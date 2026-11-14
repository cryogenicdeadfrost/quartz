(* Fine-grained reactive signals.
   No virtual-DOM diffing tax — just a tight dependency graph that tracks
   who read what and fires downstream observers when values actually change. *)

type observer = {
  id : int;
  mutable run : unit -> unit;
  mutable cleanups : (unit -> unit) list;
}

type 'a t = {
  mutable value : 'a;
  mutable subs : observer list;
  eq : 'a -> 'a -> bool;
}

let next_obs_id = ref 1
let active_obs : observer list ref = ref []
let batch_depth = ref 0
let pending_jobs : observer list ref = ref []

let add_pending (obs : observer) =
  if not (List.exists (fun o -> o.id = obs.id) !pending_jobs) then
    pending_jobs := obs :: !pending_jobs

let rec flush_pending () =
  match !pending_jobs with
  | [] -> ()
  | jobs ->
    pending_jobs := [];
    List.iter (fun o -> o.run ()) (List.rev jobs);
    if !pending_jobs <> [] then flush_pending ()

let batch fn =
  incr batch_depth;
  Fun.protect
    ~finally:(fun () ->
      decr batch_depth;
      if !batch_depth = 0 then flush_pending ())
    fn

let untrack fn =
  let saved = !active_obs in
  active_obs := [];
  Fun.protect ~finally:(fun () -> active_obs := saved) fn

let make ?(eq = ( = )) value =
  { value; subs = []; eq }

let get s =
  (match !active_obs with
   | obs :: _ ->
     if not (List.exists (fun o -> o.id = obs.id) s.subs) then (
       s.subs <- obs :: s.subs;
       obs.cleanups <-
         (fun () -> s.subs <- List.filter (fun o -> o.id <> obs.id) s.subs)
         :: obs.cleanups
     )
   | [] -> ());
  s.value

let peek s = s.value

let set s new_val =
  if not (s.eq s.value new_val) then (
    s.value <- new_val;
    let snapshot = s.subs in
    if !batch_depth > 0 then
      List.iter add_pending snapshot
    else (
      incr batch_depth;
      Fun.protect
        ~finally:(fun () ->
          decr batch_depth;
          if !batch_depth = 0 then flush_pending ())
        (fun () -> List.iter (fun o -> o.run ()) (List.rev snapshot))
    )
  )

let update s fn =
  set s (fn s.value)

(* React-style tuple hook for folks muscle-memorized into `const [x, setX] = useState(0)` *)
let create ?eq init =
  let s = make ?eq init in
  (s, set s)

let cleanup_observer (obs : observer) =
  let fns = obs.cleanups in
  obs.cleanups <- [];
  List.iter (fun f -> f ()) fns

type effect_handle = observer

let watch (fn : unit -> unit) : effect_handle =
  let id = !next_obs_id in
  incr next_obs_id;
  let rec obs = {
    id;
    run = (fun () ->
      cleanup_observer obs;
      active_obs := obs :: !active_obs;
      Fun.protect
        ~finally:(fun () ->
          match !active_obs with
          | _ :: rest -> active_obs := rest
          | [] -> ())
        fn);
    cleanups = [];
  } in
  obs.run ();
  obs

let effect (fn : unit -> unit) : unit =
  ignore (watch fn)

let dispose_effect (obs : effect_handle) =
  cleanup_observer obs;
  obs.run <- (fun () -> ())

type 'a computed = 'a t

let computed ?eq (fn : unit -> 'a) : 'a computed =
  let initial = untrack fn in
  let out = make ?eq initial in
  let initialized = ref false in
  ignore
    (watch (fun () ->
       let v = fn () in
       if !initialized then set out v
       else (
         out.value <- v;
         initialized := true
       )));
  out

let read (c : 'a computed) = get c

(* Slice/lens helper so you can zoom into a nested record field reactively *)
let lens (parent : 'a t) ~(get_field : 'a -> 'b) ~(set_field : 'a -> 'b -> 'a) =
  let derived = computed (fun () -> get_field (get parent)) in
  let write_slice new_b = update parent (fun cur -> set_field cur new_b) in
  (derived, write_slice)

let create_store init reducer =
  let st = make init in
  let dispatch action = update st (fun cur -> reducer cur action) in
  (st, dispatch)

(* undo/redo time-travel signal wrapper — handy for editors and interactive playgrounds *)
type 'a history = {
  current : 'a t;
  mutable past : 'a list;
  mutable future : 'a list;
}

let make_history init =
  { current = make init; past = []; future = [] }

let history_get h = get h.current

let history_set h v =
  let prev = peek h.current in
  if prev <> v then (
    h.past <- prev :: h.past;
    h.future <- [];
    set h.current v
  )

let undo h =
  match h.past with
  | [] -> false
  | prev :: rest ->
    let cur = peek h.current in
    h.past <- rest;
    h.future <- cur :: h.future;
    set h.current prev;
    true

let redo h =
  match h.future with
  | [] -> false
  | next_v :: rest ->
    let cur = peek h.current in
    h.future <- rest;
    h.past <- cur :: h.past;
    set h.current next_v;
    true

let ( !~ ) = get
let ( <~ ) = set
