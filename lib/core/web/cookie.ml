(* HTTP Cookie Parser/Builder, HMAC-SHA256 Signed Cookies, and
   Per-Visitor Session Manager fixing single-global-state sharing. *)

type same_site =
  | Lax
  | Strict
  | None_site

type cookie_opts = {
  max_age : int option;
  path : string;
  domain : string option;
  http_only : bool;
  secure : bool;
  same_site : same_site;
}

let default_opts = {
  max_age = Some 86400;
  path = "/";
  domain = None;
  http_only = true;
  secure = false;
  same_site = Lax;
}

let same_site_str = function
  | Lax -> "Lax"
  | Strict -> "Strict"
  | None_site -> "None"

let parse_header (raw : string) : (string * string) list =
  if String.trim raw = "" then []
  else
    String.split_on_char ';' raw
    |> List.filter_map (fun part ->
      let p = String.trim part in
      match String.index_opt p '=' with
      | None -> None
      | Some idx ->
        let k = String.trim (String.sub p 0 idx) in
        let v = String.trim (String.sub p (idx + 1) (String.length p - idx - 1)) in
        if k = "" then None else Some (k, v))

let get name raw_header = List.assoc_opt name (parse_header raw_header)

let format_set_cookie ?(opts = default_opts) ~name ~value () : string =
  let parts = ref [ Printf.sprintf "%s=%s" name value ] in
  (match opts.max_age with
   | Some ma -> parts := !parts @ [ Printf.sprintf "Max-Age=%d" ma ]
   | None -> ());
  if opts.path <> "" then parts := !parts @ [ "Path=" ^ opts.path ];
  (match opts.domain with
   | Some d -> parts := !parts @ [ "Domain=" ^ d ]
   | None -> ());
  if opts.http_only then parts := !parts @ [ "HttpOnly" ];
  if opts.secure then parts := !parts @ [ "Secure" ];
  parts := !parts @ [ "SameSite=" ^ same_site_str opts.same_site ];
  String.concat "; " !parts

let clear_cookie ?(path = "/") name =
  format_set_cookie
    ~opts:{ default_opts with max_age = Some 0; path }
    ~name ~value:"" ()

(* Signed cookies: payload_b64url.sig_b64url *)
let sign ~secret ~value : string =
  let p64 = Crypto_util.base64url_encode value in
  let mac = Crypto_util.base64url_encode (Crypto_util.hmac_sha256_bytes ~secret p64) in
  p64 ^ "." ^ mac

let verify ~secret (token : string) : string option =
  match String.split_on_char '.' token with
  | [ p64; sig64 ] when p64 <> "" && sig64 <> "" ->
    let expected =
      Crypto_util.base64url_encode (Crypto_util.hmac_sha256_bytes ~secret p64)
    in
    if Crypto_util.equal_const_time expected sig64 then
      Crypto_util.base64url_decode p64
    else None
  | _ -> None

(* Per-visitor Session Store parameterized over CLOCK & STORAGE *)
module Session = struct
  type session_data = {
    sid : string;
    csrf_token : string;
    created_at : float;
    mutable last_seen : float;
    mutable data : (string * string) list;
  }

  let encode_session s =
    Json.to_string
      (Json.obj
         [
           ("sid", Json.string s.sid);
           ("csrf_token", Json.string s.csrf_token);
           ("created_at", Json.float s.created_at);
           ("last_seen", Json.float s.last_seen);
           ("data", Json.obj (List.map (fun (k, v) -> (k, Json.string v)) s.data));
         ])

  let decode_session raw =
    match Json.parse raw with
    | Ok (Json.Object _ as root) ->
      let open Json in
      (match
         ( Option.bind (get_field "sid" root) get_string,
           Option.bind (get_field "csrf_token" root) get_string,
           Option.bind (get_field "created_at" root) get_float,
           Option.bind (get_field "last_seen" root) get_float )
       with
       | Some sid, Some csrf_token, Some created_at, Some last_seen ->
         let data =
           match get_field "data" root with
           | Some (Object kvs) ->
             List.filter_map
               (fun (k, v) -> Option.map (fun sv -> (k, sv)) (get_string v))
               kvs
           | _ -> []
         in
         Some { sid; csrf_token; created_at; last_seen; data }
       | _ -> None)
    | _ -> None

  module Make (C : Io.CLOCK) (S : Io.STORAGE with type 'a io = 'a) = struct
    type t = {
      cookie_name : string;
      secret : string;
      ttl_s : int;
      cookie_opts : cookie_opts;
    }

    let create_manager
        ?(cookie_name = "quartz_sid")
        ?(ttl_s = 86400)
        ?(secure = false)
        ~secret
        () =
      {
        cookie_name;
        secret;
        ttl_s;
        cookie_opts = { default_opts with max_age = Some ttl_s; secure };
      }

    let storage_key sid = "session:" ^ sid

    let new_session mgr =
      let now = C.now_s () in
      let sid = Crypto_util.random_hex 24 in
      let csrf_token = Crypto_util.random_hex 16 in
      let s = { sid; csrf_token; created_at = now; last_seen = now; data = [] } in
      S.set ~ttl_s:mgr.ttl_s (storage_key sid) (encode_session s);
      s

    let load_or_create mgr ~cookie_header : session_data * bool =
      let raw_tok = get mgr.cookie_name cookie_header in
      let loaded =
        match raw_tok with
        | None -> None
        | Some tok ->
          (match verify ~secret:mgr.secret tok with
           | None -> None
           | Some sid ->
             (match S.get (storage_key sid) with
              | None -> None
              | Some raw_json ->
                (match decode_session raw_json with
                 | None -> None
                 | Some s ->
                   let now = C.now_s () in
                   if now -. s.last_seen > float_of_int mgr.ttl_s then (
                     S.delete (storage_key sid);
                     None
                   ) else (
                     s.last_seen <- now;
                     S.set ~ttl_s:mgr.ttl_s (storage_key sid) (encode_session s);
                     Some s
                   ))))
      in
      match loaded with
      | Some s -> (s, false)
      | None -> (new_session mgr, true)

    let save mgr (s : session_data) =
      s.last_seen <- C.now_s ();
      S.set ~ttl_s:mgr.ttl_s (storage_key s.sid) (encode_session s)

    let destroy _mgr (s : session_data) =
      S.delete (storage_key s.sid)

    let get_val k s = List.assoc_opt k s.data

    let set_val mgr k v s =
      s.data <- (k, v) :: List.remove_assoc k s.data;
      save mgr s

    let remove_val mgr k s =
      s.data <- List.remove_assoc k s.data;
      save mgr s

    let set_cookie_header mgr s =
      let signed = sign ~secret:mgr.secret ~value:s.sid in
      format_set_cookie ~opts:mgr.cookie_opts ~name:mgr.cookie_name ~value:signed ()

    let evict_idle mgr =
      let now = C.now_s () in
      let all = S.list_prefix "session:" in
      let removed = ref 0 in
      List.iter
        (fun (k, raw) ->
          match decode_session raw with
          | Some s when now -. s.last_seen <= float_of_int mgr.ttl_s -> ()
          | _ ->
            S.delete k;
            incr removed)
        all;
      !removed
  end
end
