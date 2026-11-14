type sub = {
  id : int;
  run : unit -> unit;
}

type 'a t = {
  mutable v : 'a;
  eq : 'a -> 'a -> bool;
  mutable subs : sub list;
}

let next_sub_id = ref 1
let fresh_id () =
  let x = !next_sub_id in
  incr next_sub_id;
  x

(* active observer stack so Signal.get knows who's snooping *)
let tracker_stack : sub list ref = ref []
let batch_depth = ref 0
let pending_q : sub list ref = ref []

let enqueue_sub (s : sub) =
  if not (List.exists (fun x -> x.id = s.id) !pending_q) then
    pending_q := s :: !pending_q

let rec flush_pending () =
  match !pending_q with
  | [] -> ()
  | jobs ->
    pending_q := [];
    List.iter (fun j -> j.run ()) (List.rev jobs);
    if !pending_q <> [] then flush_pending ()

let make ?(equals = ( = )) init =
  { v = init; eq = equals; subs = [] }

let peek s = s.v

let get s =
  (match !tracker_stack with
   | [] -> ()
   | top :: _ ->
     if not (List.exists (fun x -> x.id = top.id) s.subs) then
       s.subs <- top :: s.subs);
  s.v

let set s new_v =
  if not (s.eq s.v new_v) then (
    s.v <- new_v;
    let snapshot = s.subs in
    if !batch_depth > 0 then
      List.iter enqueue_sub snapshot
    else
      List.iter (fun sub -> sub.run ()) snapshot
  )

let update s fn = set s (fn s.v)

let create ?equals init =
  let s = make ?equals init in
  (s, fun v -> set s v)

(* wait if fn throws inside with_tracker and we don't pop tracker_stack,
   every future signal read subscribes to a dead observer. fun times. *)
let with_tracker sub fn =
  tracker_stack := sub :: !tracker_stack;
  match fn () with
  | res ->
    tracker_stack := List.tl !tracker_stack;
    res
  | exception exn ->
    tracker_stack := List.tl !tracker_stack;
    raise exn

let untrack fn =
  let saved = !tracker_stack in
  tracker_stack := [];
  match fn () with
  | res ->
    tracker_stack := saved;
    res
  | exception exn ->
    tracker_stack := saved;
    raise exn

let batch fn =
  incr batch_depth;
  match fn () with
  | res ->
    decr batch_depth;
    if !batch_depth = 0 then flush_pending ();
    res
  | exception exn ->
    decr batch_depth;
    if !batch_depth = 0 then flush_pending ();
    raise exn

type effect_id = {
  mutable alive : bool;
  mutable cleanup : (unit -> unit) option;
}

let dispose_effect h =
  h.alive <- false;
  match h.cleanup with
  | None -> ()
  | Some clean ->
    h.cleanup <- None;
    clean ()

let effect (fn : unit -> (unit -> unit) option) =
  let h = { alive = true; cleanup = None } in
  let id = fresh_id () in
  let rec runner () =
    if h.alive then (
      (match h.cleanup with
       | None -> ()
       | Some c ->
         h.cleanup <- None;
         c ());
      let sub = { id; run = runner } in
      h.cleanup <- with_tracker sub fn
    )
  in
  runner ();
  h

let watch (fn : unit -> unit) =
  effect (fun () ->
    fn ();
    None)

type 'a computed = 'a t

let computed ?equals (fn : unit -> 'a) : 'a computed =
  let init_val = untrack fn in
  let out = make ?equals init_val in
  let id = fresh_id () in
  let rec recompute () =
    let sub = { id; run = recompute } in
    let next_v = with_tracker sub fn in
    set out next_v
  in
  let boot_sub = { id; run = recompute } in
  let first_v = with_tracker boot_sub fn in
  set out first_v;
  out

let read (c : 'a computed) = get c

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
