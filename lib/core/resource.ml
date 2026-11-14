(* Async Suspense resources, Stale-While-Revalidate (SWR) cache, and
   optimistic mutations with automatic rollback on error.
   Think TanStack Query + React <Suspense>, minus the Promise spaghetti. *)

type 'a status =
  | Loading
  | Ready of 'a
  | Boom of string

type 'a t = {
  key : string;
  state : 'a status Signal.t;
  fetcher : unit -> ('a, string) result;
  mutable updated_at : float;
}

(* Tiny in-memory SWR cache keyed by resource name *)
let swr_cache : (string, string) Hashtbl.t = Hashtbl.create 32

let make ?(initial : 'a status option) ~key (fetcher : unit -> ('a, string) result) : 'a t =
  let init_st =
    match initial with
    | Some st -> st
    | None ->
      (match fetcher () with
       | Ok v -> Ready v
       | Error msg -> Boom msg)
  in
  {
    key;
    state = Signal.make init_st;
    fetcher;
    updated_at = Unix.gettimeofday ();
  }

let status (r : 'a t) : 'a status =
  Signal.get r.state

let set_loading (r : 'a t) =
  Signal.set r.state Loading

let refetch (r : 'a t) =
  match r.fetcher () with
  | Ok v ->
    r.updated_at <- Unix.gettimeofday ();
    Hashtbl.replace swr_cache r.key "fresh";
    Signal.set r.state (Ready v)
  | Error e ->
    Signal.set r.state (Boom e)

(* Optimistic UI mutation: immediately apply `optimistic_val` to the UI,
   then run `commit_fn ()`. If the backend says nope (`Error msg`), we
   automatically roll back to the previous value so the UI never lies. *)
let mutate_optimistic (r : 'a t) ~optimistic_val (commit_fn : unit -> ('a, string) result) : bool =
  let prev = Signal.peek r.state in
  Signal.set r.state (Ready optimistic_val);
  match commit_fn () with
  | Ok confirmed ->
    r.updated_at <- Unix.gettimeofday ();
    Signal.set r.state (Ready confirmed);
    true
  | Error _ ->
    (* rollback! *)
    Signal.set r.state prev;
    false
