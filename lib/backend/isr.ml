(* Next.js-style Incremental Static Regeneration (ISR) in-memory cache.
   Serves cached HTML in <5 microseconds and re-renders when TTL expires. *)

type entry = {
  mutable html : string;
  mutable generated_at : float;
  revalidate_sec : float;
}

type stats = {
  mutable hits : int;
  mutable misses : int;
  mutable revalidations : int;
}

let cache : (string, entry) Hashtbl.t = Hashtbl.create 32
let telemetry = { hits = 0; misses = 0; revalidations = 0 }

let get_or_render ~path ?(revalidate_sec = 30.0) (render_fn : unit -> string) : string * string =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt cache path with
  | None ->
    telemetry.misses <- telemetry.misses + 1;
    let fresh = render_fn () in
    Hashtbl.replace cache path { html = fresh; generated_at = now; revalidate_sec };
    (fresh, "MISS")
  | Some e ->
    if now -. e.generated_at <= e.revalidate_sec then (
      telemetry.hits <- telemetry.hits + 1;
      (e.html, "HIT")
    ) else (
      telemetry.revalidations <- telemetry.revalidations + 1;
      let fresh = render_fn () in
      e.html <- fresh;
      e.generated_at <- now;
      (fresh, "REVALIDATED")
    )

let invalidate path = Hashtbl.remove cache path
let clear () = Hashtbl.clear cache
let get_stats () = telemetry
