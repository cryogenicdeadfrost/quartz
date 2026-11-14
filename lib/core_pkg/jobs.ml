(* Background Job Queue, Retries + Backoff, and Cron Scheduler (`Quartz.Jobs`).
   Generates /api/__cron/<name> webhooks for serverless hosts and runs
   in-process for long-lived Unix/Mirage servers. *)

type job_status =
  | Pending
  | Completed
  | Dead_letter of string

type job = {
  id : int;
  name : string;
  payload : string;
  mutable attempts : int;
  max_retries : int;
  mutable next_run_at : float;
  mutable status : job_status;
}

type cron_task = {
  cron_name : string;
  every_sec : float;
  mutable last_run : float;
  run_fn : unit -> (unit, string) result;
}

let next_id = ref 1
let queue : job list ref = ref []
let crons : (string, cron_task) Hashtbl.t = Hashtbl.create 16

let enqueue ?(clock = Io.Clock_hook.now_s) ?(max_retries = 3) ~name payload =
  let id = !next_id in
  incr next_id;
  let j =
    {
      id;
      name;
      payload;
      attempts = 0;
      max_retries;
      next_run_at = clock ();
      status = Pending;
    }
  in
  queue := !queue @ [ j ];
  j

let schedule_cron ~name ~every_sec fn =
  Hashtbl.replace crons name { cron_name = name; every_sec; last_run = 0.0; run_fn = fn }

let process_queue ?(clock = Io.Clock_hook.now_s) (workers : (string * (string -> (unit, string) result)) list) =
  let now = clock () in
  let ran = ref 0 in
  List.iter
    (fun j ->
      if j.status = Pending && j.next_run_at <= now then (
        incr ran;
        j.attempts <- j.attempts + 1;
        match List.assoc_opt j.name workers with
        | None -> j.status <- Dead_letter ("No handler for job " ^ j.name)
        | Some handler ->
          (match handler j.payload with
           | Ok () -> j.status <- Completed
           | Error err ->
             if j.attempts >= j.max_retries then j.status <- Dead_letter err
             else j.next_run_at <- now +. Float.pow 2.0 (float_of_int j.attempts))
      ))
    !queue;
  !ran

let tick_crons ?(clock = Io.Clock_hook.now_s) () : (string * (unit, string) result) list =
  let now = clock () in
  let results = ref [] in
  Hashtbl.iter
    (fun name task ->
      if now -. task.last_run >= task.every_sec then (
        task.last_run <- now;
        results := (name, task.run_fn ()) :: !results
      ))
    crons;
  List.rev !results

let trigger_cron_webhook ~name : (unit, string) result option =
  match Hashtbl.find_opt crons name with
  | None -> None
  | Some task -> Some (task.run_fn ())

let pending_jobs () = List.filter (fun j -> j.status = Pending) !queue
let dead_letter_jobs () =
  List.filter (fun j -> match j.status with Dead_letter _ -> true | _ -> false) !queue
let clear () =
  queue := [];
  Hashtbl.clear crons
