(* Persistent Server-Side JavaScript / NPM Worker Pool (`Quartz.Npm_server_plug`).
   Implements `Io.PLUG_TRANSPORT` over Node.js, Bun, or Deno stdin/stdout NDJSON
   so normal OCaml backend handlers can call any npm package synchronously without
   manual FFI ports. Zero third-party OCaml dependencies. *)

open Quartz_core

type worker = {
  ic : in_channel;
  oc : out_channel;
  pid : int;
  runtime : string;
  mu : Mutex.t;
  mutable closed : bool;
}

let active_worker : worker option ref = ref None
let worker_mu = Mutex.create ()

let command_exists bin =
  let cmd = Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote bin) in
  Sys.command cmd = 0

let detect_js_runtime () : string option =
  if command_exists "bun" then Some "bun"
  else if command_exists "node" then Some "node"
  else if Sys.file_exists "/mnt/c/Program Files/nodejs/node.exe" then
    Some "/mnt/c/Program Files/nodejs/node.exe"
  else if command_exists "deno" then Some "deno"
  else None

(* Inline NDJSON RPC server script executed inside Node / Bun without temp-file path issues *)
let worker_bootstrap_js =
  {|const readline = require('readline');
const path = require('path');
const { pathToFileURL } = require('url');
globalThis.__quartzState = globalThis.__quartzState || {};

async function resolvePkg(pkg) {
  if (pkg === '__builtin_eval') return null;
  const resolved = (pkg.startsWith('.') || pkg.startsWith('/'))
    ? pathToFileURL(path.resolve(pkg)).href
    : pkg;
  try { return require(pkg); }
  catch (_) { return await import(resolved); }
}

const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
rl.on('line', async (line) => {
  if (!line.trim()) return;
  let req;
  try { req = JSON.parse(line); }
  catch (e) {
    process.stdout.write(JSON.stringify({ id: 0, ok: false, error: 'Invalid JSON' }) + '\n');
    return;
  }
  try {
    let result;
    if (req.pkg === '__builtin_eval') {
      const fn = new Function('args', 'require', 'state', 'return (' + req.fn + ')(args, require, state);');
      result = await fn(req.args, require, globalThis.__quartzState);
    } else {
      const mod = await resolvePkg(req.pkg);
      const target = req.fn === 'default' ? (mod.default || mod) : (mod[req.fn] || (mod.default && mod.default[req.fn]));
      if (typeof target !== 'function') throw new Error('Export not a function: ' + req.fn);
      const callArgs = Array.isArray(req.args) ? req.args : [req.args];
      result = await target(...callArgs);
    }
    process.stdout.write(JSON.stringify({ id: req.id, ok: true, result: result === undefined ? null : result }) + '\n');
  } catch (err) {
    process.stdout.write(JSON.stringify({ id: req.id, ok: false, error: String(err && err.message ? err.message : err) }) + '\n');
  }
});
|}

let stop_worker () =
  Mutex.lock worker_mu;
  (match !active_worker with
   | Some w when not w.closed ->
     w.closed <- true;
     (try close_out_noerr w.oc with _ -> ());
     (try close_in_noerr w.ic with _ -> ());
     (try Unix.kill w.pid Sys.sigterm with _ -> ());
     (try ignore (Unix.waitpid [] w.pid) with _ -> ());
     active_worker := None
   | _ -> active_worker := None);
  Mutex.unlock worker_mu

let start_worker ?runtime () : (worker, string) result =
  let rt_opt = match runtime with Some r -> Some r | None -> detect_js_runtime () in
  match rt_opt with
  | None -> Error "No JavaScript runtime found on PATH (expected node, bun, or deno)"
  | Some bin ->
    let args =
      let base = Filename.basename bin in
      if base = "deno" then [| bin; "run"; "--allow-all"; "-e"; worker_bootstrap_js |]
      else if base = "node" || base = "node.exe" then [| bin; "--no-warnings"; "-e"; worker_bootstrap_js |]
      else [| bin; "-e"; worker_bootstrap_js |]
    in
    try
      let in_r, in_w = Unix.pipe () in
      let out_r, out_w = Unix.pipe () in
      let pid = Unix.create_process bin args out_r in_w Unix.stderr in
      Unix.close in_w;
      Unix.close out_r;
      let w =
        {
          ic = Unix.in_channel_of_descr in_r;
          oc = Unix.out_channel_of_descr out_w;
          pid;
          runtime = bin;
          mu = Mutex.create ();
          closed = false;
        }
      in
      Ok w
    with exn -> Error (Printexc.to_string exn)

let next_req_id = ref 0

let get_or_start_worker () : (worker, string) result =
  Mutex.lock worker_mu;
  let res =
    match !active_worker with
    | Some w when not w.closed -> Ok w
    | _ ->
      (match start_worker () with
       | Ok w ->
         active_worker := Some w;
         Ok w
       | Error e -> Error e)
  in
  Mutex.unlock worker_mu;
  res

let invoke_with_worker (w : worker) ~(pkg : string) ~(fn : string) ~(args_json : string)
    : (string, string) result =
  Mutex.lock w.mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock w.mu)
    (fun () ->
      if w.closed then Error "Worker already stopped"
      else
        let args_v =
          match Json.parse args_json with
          | Ok j -> j
          | Error _ -> Json.string args_json
        in
        incr next_req_id;
        let rid = !next_req_id in
        let payload =
          Json.to_string
            (Json.obj
               [
                 ("id", Json.int rid);
                 ("pkg", Json.string pkg);
                 ("fn", Json.string fn);
                 ("args", args_v);
               ])
        in
        try
          output_string w.oc (payload ^ "\n");
          flush w.oc;
          let line = input_line w.ic in
          match Json.parse line with
          | Error e -> Error ("Worker returned invalid JSON: " ^ e)
          | Ok j ->
            let ok = Option.bind (Json.get_field "ok" j) Json.get_bool |> Option.value ~default:false in
            if ok then
              let res_j = Option.value ~default:Json.null (Json.get_field "result" j) in
              Ok (Json.to_string res_j)
            else
              let err =
                Option.bind (Json.get_field "error" j) Json.get_string
                |> Option.value ~default:"Unknown JS plug error"
              in
              Error err
        with exn ->
          w.closed <- true;
          Error ("Worker I/O failed: " ^ Printexc.to_string exn))

let invoke ~(pkg : string) ~(fn : string) ~(args_json : string) : (string, string) result =
  match get_or_start_worker () with
  | Error e -> Error e
  | Ok w -> invoke_with_worker w ~pkg ~fn ~args_json

let eval_js ~(expr_fn : string) ~(args_json : string) : (string, string) result =
  invoke ~pkg:"__builtin_eval" ~fn:expr_fn ~args_json

module Worker_transport : Io.PLUG_TRANSPORT = struct
  let invoke = invoke
end
