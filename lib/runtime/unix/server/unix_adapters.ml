(* Concrete Unix & Posix Threads Runtime Adapters (`quartz.unix`).
   Installs wall clock & CSPRNG hooks on load and provides persistent
   on-disk WAL storage and a bounded multi-threaded worker pool. *)

open Quartz_core

let () =
  Io.Clock_hook.set Unix.gettimeofday;
  Crypto_util.set_random_bytes_provider (fun n ->
    try
      let ic = open_in_bin "/dev/urandom" in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let buf = Bytes.create n in
          really_input ic buf 0 n;
          Bytes.unsafe_to_string buf)
    with _ ->
      let b = Bytes.create n in
      let seed = int_of_float (Unix.gettimeofday () *. 1_000_000.0) in
      let st = Random.State.make [| seed; Unix.getpid () |] in
      for i = 0 to n - 1 do
        Bytes.set b i (Char.chr (Random.State.int st 256))
      done;
      Bytes.unsafe_to_string b)

module Unix_clock : Io.CLOCK = struct
  let now_s () = Unix.gettimeofday ()
  let now_us () = Int64.of_float (Unix.gettimeofday () *. 1_000_000.0)
end

(* Crash-safe On-Disk Write-Ahead Log Storage Engine *)
module File_storage = struct
  module Mem = Store.Wal_store (Unix_clock)

  type t = {
    wal_path : string;
    state : Mem.t;
    mu : Mutex.t;
  }

  let append_line path line =
    let oc = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o644 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc line;
        output_char oc '\n';
        flush oc)

  let open_wal ~wal_path : t =
    let state = Mem.create () in
    if Sys.file_exists wal_path then (
      let ic = open_in_bin wal_path in
      let raw =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      ignore (Mem.replay_wal_string state raw)
    );
    { wal_path; state; mu = Mutex.create () }

  let get t key =
    Mutex.lock t.mu;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mu) (fun () -> Mem.get_on t.state key)

  let set t ?ttl_s key value =
    Mutex.lock t.mu;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock t.mu)
      (fun () ->
        Mem.set_on t.state ?ttl_s key value;
        let exp = Option.map (fun s -> Unix_clock.now_s () +. float_of_int s) ttl_s in
        append_line t.wal_path (Mem.encode_op (Mem.Op_set (key, value, exp))))

  let delete t key =
    Mutex.lock t.mu;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock t.mu)
      (fun () ->
        Mem.delete_on t.state key;
        append_line t.wal_path (Mem.encode_op (Mem.Op_del key)))

  let list_prefix t prefix =
    Mutex.lock t.mu;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mu) (fun () -> Mem.list_prefix_on t.state prefix)

  let compact t =
    Mutex.lock t.mu;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock t.mu)
      (fun () ->
        let kept = Mem.compact t.state in
        let snap = Mem.dump_snapshot t.state in
        let tmp = t.wal_path ^ ".tmp" in
        let oc = open_out_bin tmp in
        output_string oc snap;
        if snap <> "" then output_char oc '\n';
        close_out oc;
        Sys.rename tmp t.wal_path;
        kept)

  let as_storage (t : t) : (module Io.SYNC_STORAGE) =
    (module struct
      type 'a io = 'a
      let get k = get t k
      let set ?ttl_s k v = set t ?ttl_s k v
      let delete k = delete t k
      let list_prefix p = list_prefix t p
      let append_log ~topic entry =
        append_line t.wal_path (Mem.encode_op (Mem.Op_log (topic, entry)))
      let read_log ~topic:_ = []
    end)
end

(* Bounded Multi-Threaded Worker Pool *)
module Worker_pool = struct
  type t = {
    tasks : (unit -> unit) Queue.t;
    mu : Mutex.t;
    cond : Condition.t;
    mutable stopped : bool;
    threads : Thread.t list;
  }

  let create ?(workers = 8) () =
    let tasks = Queue.create () in
    let mu = Mutex.create () in
    let cond = Condition.create () in
    let pool = ref None in
    let worker_loop () =
      let running = ref true in
      while !running do
        Mutex.lock mu;
        while Queue.is_empty tasks && not (match !pool with Some p -> p.stopped | None -> false) do
          Condition.wait cond mu
        done;
        let next_task =
          if Queue.is_empty tasks then None else Some (Queue.pop tasks)
        in
        let is_stopped = match !pool with Some p -> p.stopped | None -> false in
        Mutex.unlock mu;
        match next_task with
        | Some fn -> (try fn () with _ -> ())
        | None -> if is_stopped then running := false
      done
    in
    let p = { tasks; mu; cond; stopped = false; threads = [] } in
    pool := Some p;
    let ths = List.init (max 1 workers) (fun _ -> Thread.create worker_loop ()) in
    let p_final = { p with threads = ths } in
    pool := Some p_final;
    p_final

  let submit p fn =
    Mutex.lock p.mu;
    Queue.push fn p.tasks;
    Condition.signal p.cond;
    Mutex.unlock p.mu

  let shutdown p =
    Mutex.lock p.mu;
    p.stopped <- true;
    Condition.broadcast p.cond;
    Mutex.unlock p.mu;
    List.iter Thread.join p.threads
end
