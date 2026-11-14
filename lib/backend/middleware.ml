(* Functional HTTP Middleware Pipeline (Express / NestJS / Hono style,
   except without mutating a mystery `req` object 14 times down the stack). *)

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

let ok ?(content_type = "application/json") ?(headers = []) body =
  { status = "200 OK"; content_type; headers; body }

let html ?(headers = []) body =
  { status = "200 OK"; content_type = "text/html"; headers; body }

let reject ~status ?(content_type = "application/json") msg =
  {
    status;
    content_type;
    headers = [];
    body = Printf.sprintf {|{"error":"%s"}|} msg;
  }

let with_header k v (r : res) =
  { r with headers = (k, v) :: List.remove_assoc k r.headers }

(* Compose a list of middlewares around a base handler *)
let pipeline (mws : t list) (final_handler : handler) : handler =
  List.fold_right (fun mw next -> mw next) mws final_handler

let header_opt key (r : req) =
  let k_low = String.lowercase_ascii key in
  List.find_map
    (fun (hk, hv) -> if String.lowercase_ascii hk = k_low then Some hv else None)
    r.headers

(* 1. CORS Middleware *)
let cors ?(origin = "*") ?(methods = "GET, POST, PUT, DELETE, OPTIONS") () : t =
  fun next req ->
    if req.meth = "OPTIONS" then
      ok ~headers:[
        ("Access-Control-Allow-Origin", origin);
        ("Access-Control-Allow-Methods", methods);
        ("Access-Control-Allow-Headers", "Content-Type, Authorization, X-CSRF-Token");
      ] ""
    else
      let resp = next req in
      resp
      |> with_header "Access-Control-Allow-Origin" origin
      |> with_header "Access-Control-Allow-Methods" methods

(* 2. Token-Bucket Rate Limiter (`rate_cop`) -- O(1) per client IP *)
type bucket = {
  mutable tokens : float;
  mutable last_ts : float;
}

let rate_cop ?(max_burst = 60.0) ?(refill_per_sec = 20.0) () : t =
  let table : (string, bucket) Hashtbl.t = Hashtbl.create 64 in
  fun next req ->
    let now = Unix.gettimeofday () in
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
      reject ~status:"429 Too Many Requests" "Rate limit exceeded -- chill out for a sec"
    else (
      b.tokens <- b.tokens -. 1.0;
      next req
      |> with_header "X-RateLimit-Remaining" (string_of_int (int_of_float b.tokens))
    )

(* 3. Bearer Token Auth Guard (`bearer_guard`) *)
let bearer_guard ~token : t =
  let expected = "Bearer " ^ token in
  fun next req ->
    match header_opt "authorization" req with
    | Some v when String.trim v = expected -> next req
    | _ -> reject ~status:"401 Unauthorized" "Missing or invalid Bearer token"

(* 4. CSRF Shield for state-mutating POST/PUT/DELETE requests *)
let csrf_shield ?(header_name = "x-csrf-token") ~expected_token () : t =
  fun next req ->
    if req.meth = "GET" || req.meth = "HEAD" || req.meth = "OPTIONS" then
      next req
    else
      match header_opt header_name req with
      | Some t when String.trim t = expected_token -> next req
      | _ -> reject ~status:"403 Forbidden" "CSRF token mismatch"

(* 5. Standard Security Headers *)
let security_headers () : t =
  fun next req ->
    next req
    |> with_header "X-Content-Type-Options" "nosniff"
    |> with_header "X-Frame-Options" "SAMEORIGIN"
    |> with_header "Referrer-Policy" "strict-origin-when-cross-origin"

(* 6. Microsecond Execution Timer Header (`X-Quartz-Time-Us`) *)
let req_timer () : t =
  fun next req ->
    let t0 = Unix.gettimeofday () in
    let resp = next req in
    let us = int_of_float ((Unix.gettimeofday () -. t0) *. 1_000_000.0) in
    with_header "X-Quartz-Time-Us" (string_of_int us) resp
