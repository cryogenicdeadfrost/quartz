(* Async Suspense resources, Stale-While-Revalidate (SWR) cache, and
   optimistic mutations with automatic rollback on error.
   Zero Unix dependency: uses Io.Clock_hook by default or an explicit clock. *)

type 'a status =
  | Loading
  | Ready of 'a
  | Boom of string

type 'a t = {
  key : string;
  state : 'a status Signal.t;
  fetcher : unit -> ('a, string) result;
  clock : unit -> float;
  mutable updated_at : float;
}

let swr_cache : (string, string) Hashtbl.t = Hashtbl.create 32

let make ?(clock = Io.Clock_hook.now_s) ?(initial : 'a status option) ~key (fetcher : unit -> ('a, string) result) : 'a t =
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
    clock;
    updated_at = clock ();
  }

let status (r : 'a t) : 'a status =
  Signal.get r.state

let set_loading (r : 'a t) =
  Signal.set r.state Loading

let refetch (r : 'a t) =
  match r.fetcher () with
  | Ok v ->
    r.updated_at <- r.clock ();
    Hashtbl.replace swr_cache r.key "fresh";
    Signal.set r.state (Ready v)
  | Error e ->
    Signal.set r.state (Boom e)

let mutate_optimistic (r : 'a t) ~optimistic_val (commit_fn : unit -> ('a, string) result) : bool =
  let prev = Signal.peek r.state in
  Signal.set r.state (Ready optimistic_val);
  match commit_fn () with
  | Ok confirmed ->
    r.updated_at <- r.clock ();
    Signal.set r.state (Ready confirmed);
    true
  | Error _ ->
    Signal.set r.state prev;
    false
