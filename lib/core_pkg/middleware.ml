(* Pure Functional HTTP Middleware Pipeline, NestJS-style Guards/Interceptors,
   ETag Conditional GET, Body Limits, and End-to-End Typed RPC Endpoints.
   Zero Unix dependency: clock injected via ?clock or Io.Clock_hook. *)

type req = {
  meth : string;
  path : string;
  query : (string * string) list;
  headers : (string * string) list;
  body : string;
  client_ip : string;
}

type res = {
  status : string;
  content_type : string;
  headers : (string * string) list;
  body : string;
}

type handler = req -> res
type t = handler -> handler

let status_code_of_res (r : res) : int =
  match String.split_on_char ' ' (String.trim r.status) with
  | code :: _ -> Option.value ~default:200 (int_of_string_opt code)
  | [] -> 200

let ok ?(content_type = "application/json") ?(headers = []) body =
  { status = "200 OK"; content_type; headers; body }

let html ?(status = "200 OK") ?(headers = []) body =
  { status; content_type = "text/html"; headers; body }

let redirect ?(status = "302 Found") location =
  { status; content_type = "text/plain"; headers = [ ("Location", location) ]; body = "" }

let not_found ?(content_type = "text/html") body =
  { status = "404 Not Found"; content_type; headers = []; body }

let reject ~status ?(content_type = "application/json") msg =
  {
    status;
    content_type;
    headers = [];
    body = Json.to_string (Json.obj [ ("error", Json.string msg) ]);
  }

let with_header k v (r : res) =
  { r with headers = (k, v) :: List.remove_assoc k r.headers }

let add_header k v (r : res) =
  { r with headers = r.headers @ [ (k, v) ] }

let pipeline (mws : t list) (final_handler : handler) : handler =
  List.fold_right (fun mw next -> mw next) mws final_handler

let header_opt key (r : req) =
  let k_low = String.lowercase_ascii key in
  List.find_map
    (fun (hk, hv) -> if String.lowercase_ascii hk = k_low then Some hv else None)
    r.headers

let cors ?(origin = "*") ?(methods = "GET, POST, PUT, DELETE, OPTIONS") () : t =
  fun next req ->
    if req.meth = "OPTIONS" then
      ok ~headers:[
        ("Access-Control-Allow-Origin", origin);
        ("Access-Control-Allow-Methods", methods);
        ("Access-Control-Allow-Headers", "Content-Type, Authorization, X-CSRF-Token");
      ] ""
    else
      next req
      |> with_header "Access-Control-Allow-Origin" origin
      |> with_header "Access-Control-Allow-Methods" methods

type bucket = {
  mutable tokens : float;
  mutable last_ts : float;
}

let rate_cop ?(clock = Io.Clock_hook.now_s) ?(max_burst = 60.0) ?(refill_per_sec = 20.0) () : t =
  let table : (string, bucket) Hashtbl.t = Hashtbl.create 64 in
  fun next req ->
    let now = clock () in
    let b =
      match Hashtbl.find_opt table req.client_ip with
      | Some b ->
        let dt = Float.max 0.0 (now -. b.last_ts) in
        b.tokens <- Float.min max_burst (b.tokens +. (dt *. refill_per_sec));
        b.last_ts <- now;
        b
      | None ->
        let b = { tokens = max_burst; last_ts = now } in
        Hashtbl.replace table req.client_ip b;
        b
    in
    if b.tokens < 1.0 then
      reject ~status:"429 Too Many Requests" "Rate limit exceeded"
    else (
      b.tokens <- b.tokens -. 1.0;
      next req
      |> with_header "X-RateLimit-Remaining" (string_of_int (int_of_float b.tokens))
    )

let bearer_guard ~token : t =
  let expected = "Bearer " ^ token in
  fun next req ->
    match header_opt "authorization" req with
    | Some v when Crypto_util.equal_const_time (String.trim v) expected -> next req
    | _ -> reject ~status:"401 Unauthorized" "Missing or invalid Bearer token"

let csrf_shield ?(header_name = "x-csrf-token") ~expected_token () : t =
  fun next req ->
    if req.meth = "GET" || req.meth = "HEAD" || req.meth = "OPTIONS" then
      next req
    else
      match header_opt header_name req with
      | Some t when Crypto_util.equal_const_time (String.trim t) expected_token -> next req
      | _ -> reject ~status:"403 Forbidden" "CSRF token mismatch"

let security_headers ?(csp = "default-src 'self' 'unsafe-inline'; img-src 'self' data:; style-src 'self' 'unsafe-inline'") () : t =
  fun next req ->
    next req
    |> with_header "X-Content-Type-Options" "nosniff"
    |> with_header "X-Frame-Options" "SAMEORIGIN"
    |> with_header "Referrer-Policy" "strict-origin-when-cross-origin"
    |> with_header "Content-Security-Policy" csp

let body_limit ?(max_bytes = 1_048_576) () : t =
  fun next req ->
    if String.length req.body > max_bytes then
      reject ~status:"413 Payload Too Large"
        (Printf.sprintf "Request body exceeds %d byte limit" max_bytes)
    else next req

let etag_cache () : t =
  fun next req ->
    let resp = next req in
    if req.meth = "GET" && status_code_of_res resp = 200 then (
      let tag = "\"" ^ String.sub (Crypto_util.sha256_hex resp.body) 0 16 ^ "\"" in
      match header_opt "if-none-match" req with
      | Some client_tag when String.trim client_tag = tag ->
        { status = "304 Not Modified"; content_type = resp.content_type; headers = ("ETag", tag) :: resp.headers; body = "" }
      | _ -> with_header "ETag" tag resp
    ) else resp

(* NestJS-style Typed Guards & Interceptors *)
let guard ?(status = "403 Forbidden") (check_fn : req -> (unit, string) result) : t =
  fun next req ->
    match check_fn req with
    | Ok () -> next req
    | Error msg -> reject ~status msg

let intercept ?(before = fun r -> r) ?(after = fun _ res -> res) () : t =
  fun next req ->
    let req' = before req in
    let res = next req' in
    after req' res

let req_timer ?(clock = Io.Clock_hook.now_s) () : t =
  fun next req ->
    let t0 = clock () in
    let resp = next req in
    let us = max 0 (int_of_float ((clock () -. t0) *. 1_000_000.0)) in
    with_header "X-Quartz-Time-Us" (string_of_int us) resp

(* End-to-End Type-Safe RPC Endpoints (Next.js Server Actions / tRPC style) *)
module Endpoint = struct
  type ('req, 'res) spec = {
    name : string;
    meth : string;
    path : string;
    decode_req : 'req Json.Decode.decoder;
    encode_res : 'res -> Json.t;
  }

  type boxed = {
    b_name : string;
    b_meth : string;
    b_path : string;
    dispatch : req -> res;
  }

  let define ~name ?(meth = "POST") ?path ~decode_req ~encode_res (handle : req -> 'req -> ('res, string) result)
      : ('req, 'res) spec * boxed =
    let rpc_path = Option.value ~default:("/api/__rpc/" ^ name) path in
    let spec = { name; meth; path = rpc_path; decode_req; encode_res } in
    let box =
      {
        b_name = name;
        b_meth = meth;
        b_path = rpc_path;
        dispatch =
          (fun req ->
            match Json.Decode.decode_string decode_req req.body with
            | Error err -> reject ~status:"400 Bad Request" err
            | Ok typed_req ->
              (match handle req typed_req with
               | Ok typed_res -> ok (Json.to_string (encode_res typed_res))
               | Error err -> reject ~status:"422 Unprocessable Entity" err));
      }
    in
    (spec, box)

  let path_of s = s.path

  (* Call an endpoint directly in-process in OCaml unit tests / SSR loaders with full type safety *)
  let invoke_local (spec : ('req, 'res) spec) (box : boxed) (encode_req : 'req -> Json.t) (payload : 'req)
      : (string, string) result =
    let body = Json.to_string (encode_req payload) in
    let req = { meth = spec.meth; path = spec.path; query = []; headers = []; body; client_ip = "127.0.0.1" } in
    let res = box.dispatch req in
    if status_code_of_res res >= 400 then Error res.body else Ok res.body

  let generate_js_client (boxes : boxed list) : string =
    let methods =
      List.map
        (fun b ->
          Printf.sprintf
            "  %s: async function(payload) {\n    const r = await fetch(%S, { method: %S, headers: {'Content-Type':'application/json'}, body: JSON.stringify(payload) });\n    if (!r.ok) throw new Error(await r.text());\n    return r.json();\n  }"
            b.b_name b.b_path b.b_meth)
        boxes
    in
    "export const quartzRpc = {\n" ^ String.concat ",\n" methods ^ "\n};\n"
end
