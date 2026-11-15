(* Vercel Custom Runtime (`bootstrap`) & AWS Lambda `provided.al2023` Event Loop.
   Implements the AWS Lambda Runtime HTTP API over plain OCaml Unix TCP sockets
   with zero external HTTP client libraries (`curl` / `cohttp`). *)

open Quartz_core

let parse_host_port endpoint =
  match String.split_on_char ':' (String.trim endpoint) with
  | [ h; p ] -> (h, Option.value ~default:80 (int_of_string_opt p))
  | [ h ] -> (h, 80)
  | _ -> ("127.0.0.1", 9001)

let tcp_request ~host ~port ~meth ~path ?(headers = []) ?(body = "") () : (int * (string * string) list * string, string) result =
  try
    let addrs = Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] in
    match addrs with
    | [] -> Error ("Cannot resolve runtime host: " ^ host)
    | ai :: _ ->
      let fd = Unix.socket ai.ai_family ai.ai_socktype ai.ai_protocol in
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with _ -> ())
        (fun () ->
          Unix.connect fd ai.ai_addr;
          let hdr_lines =
            List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) headers
          in
          let req_wire =
            Printf.sprintf
              "%s %s HTTP/1.1\r\nHost: %s:%d\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
              meth path host port (String.concat "" hdr_lines) (String.length body) body
          in
          let _ = Unix.write_substring fd req_wire 0 (String.length req_wire) in
          let buf = Buffer.create 4096 in
          let tmp = Bytes.create 8192 in
          let rec read_all () =
            let n = Unix.read fd tmp 0 8192 in
            if n > 0 then (
              Buffer.add_subbytes buf tmp 0 n;
              read_all ()
            )
          in
          read_all ();
          let raw = Buffer.contents buf in
          match Http.find_crlfcrlf raw with
          | None -> Error "Malformed response from runtime API"
          | Some idx ->
            let hdr_raw = String.sub raw 0 idx in
            let resp_body = String.sub raw (idx + 4) (String.length raw - idx - 4) in
            let lines = String.split_on_char '\n' hdr_raw in
            let status_code =
              match lines with
              | st_line :: _ ->
                (match String.split_on_char ' ' (String.trim st_line) with
                 | _ :: code :: _ -> Option.value ~default:200 (int_of_string_opt code)
                 | _ -> 200)
              | [] -> 200
            in
            let resp_headers =
              match lines with
              | _ :: rest ->
                List.filter_map
                  (fun l ->
                    match String.index_opt (String.trim l) ':' with
                    | Some ci ->
                      let tl = String.trim l in
                      Some (String.trim (String.sub tl 0 ci), String.trim (String.sub tl (ci + 1) (String.length tl - ci - 1)))
                    | None -> None)
                  rest
              | [] -> []
            in
            Ok (status_code, resp_headers, resp_body))
  with exn -> Error (Printexc.to_string exn)

let req_of_lambda_event (event_json : string) : Middleware.req =
  match Json.parse event_json with
  | Error _ ->
    { Middleware.meth = "GET"; path = "/"; query = []; headers = []; body = ""; client_ip = "127.0.0.1" }
  | Ok j ->
    let ( >>= ) = Option.bind in
    (* Supports both Lambda Function URL v2 (`rawPath`, `requestContext.http.method`) and API GW / Vercel v1 *)
    let raw_body =
      Json.get_field "body" j >>= Json.get_string |> Option.value ~default:""
    in
    let parsed_j =
      (* Vercel often wraps HTTP payload inside `{ "body": "{ ... }" }` or direct API GW object *)
      match Json.get_field "httpMethod" j with
      | Some _ -> j
      | None ->
        (match Json.get_field "Action" j with
         | Some _ -> j
         | None -> j)
    in
    let meth =
      match Json.get_field "httpMethod" parsed_j >>= Json.get_string with
      | Some m -> m
      | None ->
        (match Json.get_field "requestContext" parsed_j with
         | Some rc ->
           Json.get_field "http" rc
           >>= Json.get_field "method"
           >>= Json.get_string
           |> Option.value ~default:"GET"
         | None -> "GET")
    in
    let raw_path =
      match Json.get_field "path" parsed_j >>= Json.get_string with
      | Some p -> p
      | None ->
        Json.get_field "rawPath" parsed_j >>= Json.get_string |> Option.value ~default:"/"
    in
    let raw_qs =
      Json.get_field "rawQueryString" parsed_j >>= Json.get_string |> Option.value ~default:""
    in
    let path, q1 = Http.split_url_and_query raw_path in
    let q2 = Http.parse_form_urlencoded raw_qs in
    let headers =
      match Json.get_field "headers" parsed_j with
      | Some (Json.Object kvs) ->
        List.filter_map (fun (k, v) -> Option.map (fun sv -> (k, sv)) (Json.get_string v)) kvs
      | _ -> []
    in
    let is_b64 =
      Json.get_field "isBase64Encoded" parsed_j >>= Json.get_bool |> Option.value ~default:false
    in
    let body =
      if is_b64 then Option.value ~default:raw_body (Crypto_util.base64_decode raw_body)
      else raw_body
    in
    let client_ip = Http.extract_client_ip ~peer_ip:"127.0.0.1" headers in
    { Middleware.meth = String.uppercase_ascii meth; path; query = q1 @ q2; headers; body; client_ip }

let lambda_response_of_res (res : Middleware.res) : string =
  let status_code = Middleware.status_code_of_res res in
  let all_headers =
    ("Content-Type", res.content_type) :: List.remove_assoc "Content-Type" res.headers
  in
  Json.to_string
    (Json.obj
       [
         ("statusCode", Json.int status_code);
         ("headers", Json.obj (List.map (fun (k, v) -> (k, Json.string v)) all_headers));
         ("body", Json.string res.body);
         ("isBase64Encoded", Json.bool false);
       ])

module Make
    (C : Io.CLOCK)
    (S : Io.STORAGE with type 'a io = 'a)
    (P : Io.PUBSUB) =
struct
  module Core_app = App.Make (C) (S) (P)

  let run ?max_invocations (app : Core_app.t) =
    let runtime_api =
      try Sys.getenv "AWS_LAMBDA_RUNTIME_API" with Not_found -> "127.0.0.1:9001"
    in
    let host, port = parse_host_port runtime_api in
    let processed = ref 0 in
    let keep_going () =
      match max_invocations with None -> true | Some n -> !processed < n
    in
    while keep_going () do
      incr processed;
      match tcp_request ~host ~port ~meth:"GET" ~path:"/2018-06-01/runtime/invocation/next" () with
      | Error msg ->
        Printf.eprintf "[quartz.vercel] invocation poll error: %s\n%!" msg;
        Unix.sleepf 0.25
      | Ok (_, headers, event_body) ->
        let req_id =
          Http.header_opt "lambda-runtime-aws-request-id" headers
          |> Option.value ~default:"local-req"
        in
        (try
           let req = req_of_lambda_event event_body in
           let res = Core_app.handle app req in
           let payload = lambda_response_of_res res in
           let resp_path =
             Printf.sprintf "/2018-06-01/runtime/invocation/%s/response" req_id
           in
           ignore
             (tcp_request ~host ~port ~meth:"POST" ~path:resp_path
                ~headers:[ ("Content-Type", "application/json") ]
                ~body:payload ())
         with exn ->
           let err_path =
             Printf.sprintf "/2018-06-01/runtime/invocation/%s/error" req_id
           in
           let err_payload =
             Json.to_string
               (Json.obj
                  [
                    ("errorMessage", Json.string (Printexc.to_string exn));
                    ("errorType", Json.string "QuartzRuntimeError");
                  ])
           in
           ignore
             (tcp_request ~host ~port ~meth:"POST" ~path:err_path
                ~headers:[ ("Content-Type", "application/json") ]
                ~body:err_payload ()))
    done
end
