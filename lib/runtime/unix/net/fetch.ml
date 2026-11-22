(* Outbound HTTP/HTTPS Fetch client with AbortController cancellation.
   Uses native Unix sockets for plain HTTP and falls back to curl for TLS HTTPS. *)

open Quartz_core

type response = {
  status : int;
  status_text : string;
  ok : bool;
  headers : (string * string) list;
  body : string;
}

let write_all fd s =
  let n = String.length s in
  let off = ref 0 in
  while !off < n do
    let w = Unix.write_substring fd s !off (n - !off) in
    if w <= 0 then off := n else off := !off + w
  done

let read_all ?(timeout_s = 10.0) ?abort fd =
  let buf = Buffer.create 4096 in
  let tmp = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    match abort with
    | Some s when Promise.Abort.is_aborted s ->
      Error (Option.value ~default:"AbortError" (Promise.Abort.reason s))
    | _ ->
      let rem = deadline -. Unix.gettimeofday () in
      if rem <= 0.0 then Error "TimeoutError: fetch timed out"
      else
        let r, _, _ = Unix.select [ fd ] [] [] (Float.min 0.25 rem) in
        match r with
        | [] -> loop ()
        | _ ->
          let n = Unix.read fd tmp 0 4096 in
          if n <= 0 then Ok (Buffer.contents buf)
          else (
            Buffer.add_subbytes buf tmp 0 n;
            loop ()
          )
  in
  loop ()

let parse_http_response raw : (response, string) result =
  match Http.find_crlfcrlf raw with
  | None -> Error "Malformed HTTP response: missing header terminator"
  | Some sep ->
    let head = String.sub raw 0 sep in
    let body = String.sub raw (sep + 4) (String.length raw - sep - 4) in
    let lines =
      String.split_on_char '\n' head
      |> List.map (fun l ->
           let n = String.length l in
           if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
    in
    match lines with
    | [] -> Error "Empty HTTP response"
    | status_line :: hdr_lines ->
      let parts = String.split_on_char ' ' status_line in
      let status, status_text =
        match parts with
        | _ :: code :: rest ->
          (Option.value ~default:0 (int_of_string_opt code), String.concat " " rest)
        | _ -> (0, "")
      in
      let headers =
        List.filter_map
          (fun l ->
            match String.index_opt l ':' with
            | None -> None
            | Some i ->
              let k = String.lowercase_ascii (String.trim (String.sub l 0 i)) in
              let v = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
              Some (k, v))
          hdr_lines
      in
      let final_body =
        match List.assoc_opt "transfer-encoding" headers with
        | Some te when String.lowercase_ascii te = "chunked" ->
          (match Http.decode_chunked_from ~pos:0 body with
           | Ok (decoded, _) -> decoded
           | Error _ -> body)
        | _ -> body
      in
      Ok { status; status_text; ok = status >= 200 && status < 300; headers; body = final_body }

let request_http_socket ~(u : Es_std.Url.t) ~meth ~headers ~body ~timeout_s ?abort () =
  let port = Option.value ~default:80 u.port in
  let target =
    let qs = Es_std.Search_params.to_string u.query in
    if qs = "" then u.path else u.path ^ "?" ^ qs
  in
  let host_hdr = if port = 80 then u.host else Printf.sprintf "%s:%d" u.host port in
  let has_hdr k =
    let lk = String.lowercase_ascii k in
    List.exists (fun (hk, _) -> String.lowercase_ascii hk = lk) headers
  in
  let base_hdrs =
    (if has_hdr "host" then [] else [ ("Host", host_hdr) ])
    @ (if has_hdr "connection" then [] else [ ("Connection", "close") ])
    @ (if body <> "" && not (has_hdr "content-length") then
         [ ("Content-Length", string_of_int (String.length body)) ]
       else [])
    @ headers
  in
  let hdr_wire =
    String.concat "\r\n" (List.map (fun (k, v) -> k ^ ": " ^ v) base_hdrs)
  in
  let req_wire = Printf.sprintf "%s %s HTTP/1.1\r\n%s\r\n\r\n%s" meth target hdr_wire body in
  try
    let addr =
      try Unix.inet_addr_of_string u.host
      with _ -> (Unix.gethostbyname u.host).Unix.h_addr_list.(0)
    in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Fun.protect
      ~finally:(fun () -> try Unix.close sock with _ -> ())
      (fun () ->
        Unix.connect sock (Unix.ADDR_INET (addr, port));
        write_all sock req_wire;
        match read_all ~timeout_s ?abort sock with
        | Error e -> Error e
        | Ok raw -> parse_http_response raw)
  with exn -> Error ("Fetch socket error: " ^ Printexc.to_string exn)

let request_https_curl ~url ~meth ~headers ~body ~timeout_s =
  let hdr_args =
    List.concat_map (fun (k, v) -> [ "-H"; k ^ ": " ^ v ]) headers
  in
  let body_args = if body = "" then [] else [ "--data-binary"; body ] in
  let args =
    [ "curl"; "-sS"; "-i"; "--max-time"; Printf.sprintf "%.2f" timeout_s; "-X"; meth ]
    @ hdr_args
    @ body_args
    @ [ url ]
  in
  let cmd = String.concat " " (List.map Filename.quote args) in
  try
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_string buf (input_line ic);
         Buffer.add_char buf '\n'
       done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    let raw = Buffer.contents buf in
    let normalized = String.concat "\r\n" (String.split_on_char '\n' raw) in
    parse_http_response normalized
  with exn -> Error ("Fetch HTTPS error: " ^ Printexc.to_string exn)

let request
    ?(meth = "GET")
    ?(headers = [])
    ?(body = "")
    ?(timeout_s = 10.0)
    ?abort
    (url : string) : (response, string) result =
  match abort with
  | Some s when Promise.Abort.is_aborted s ->
    Error (Option.value ~default:"AbortError" (Promise.Abort.reason s))
  | _ ->
    (match Es_std.Url.parse url with
     | Error e -> Error e
     | Ok u ->
       let m = String.uppercase_ascii meth in
       if u.scheme = "http" then request_http_socket ~u ~meth:m ~headers ~body ~timeout_s ?abort ()
       else if u.scheme = "https" then request_https_curl ~url ~meth:m ~headers ~body ~timeout_s
       else Error ("Unsupported fetch scheme: " ^ u.scheme))

let get ?headers ?timeout_s ?abort url = request ~meth:"GET" ?headers ?timeout_s ?abort url

let post_json ?(headers = []) ?timeout_s ?abort url (payload : Json.t) =
  let hdrs = ("Content-Type", "application/json") :: headers in
  request ~meth:"POST" ~headers:hdrs ~body:(Json.to_string payload) ?timeout_s ?abort url

let fetch_promise ?meth ?headers ?body ?timeout_s ?abort url : response Promise.t =
  match request ?meth ?headers ?body ?timeout_s ?abort url with
  | Ok r -> Promise.resolve r
  | Error e -> Promise.reject e
