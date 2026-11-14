(* Pure-OCaml RFC 9112 HTTP/1.1 Parser, Chunked Decoder, Range Slicer & Keep-Alive Codec.
   Zero Unix dependency -- works identically in socket loops, Lambda bootstrap, and Unikernels. *)

type parsed_request = {
  meth : string;
  target : string;
  version : string;
  headers : (string * string) list;
  body : string;
}

type parse_state =
  | Complete of parsed_request * string (* parsed request + leftover pipelined bytes *)
  | Need_more_bytes
  | Malformed of string

let find_crlfcrlf s =
  let len = String.length s in
  let rec loop i =
    if i + 3 >= len then None
    else if s.[i] = '\r' && s.[i + 1] = '\n' && s.[i + 2] = '\r' && s.[i + 3] = '\n' then
      Some i
    else loop (i + 1)
  in
  loop 0

let header_opt key headers =
  let kl = String.lowercase_ascii key in
  List.find_map
    (fun (k, v) -> if String.lowercase_ascii k = kl then Some v else None)
    headers

let url_decode s =
  let len = String.length s in
  let b = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    match s.[!i] with
    | '+' -> Buffer.add_char b ' '; incr i
    | '%' when !i + 2 < len ->
      let hex = String.sub s (!i + 1) 2 in
      (match int_of_string_opt ("0x" ^ hex) with
       | Some code -> Buffer.add_char b (Char.chr (code land 0xff)); i := !i + 3
       | None -> Buffer.add_char b '%'; incr i)
    | c -> Buffer.add_char b c; incr i
  done;
  Buffer.contents b

let parse_form_urlencoded body =
  if body = "" then []
  else
    String.split_on_char '&' body
    |> List.filter_map (fun kv ->
      match String.split_on_char '=' kv with
      | k :: rest when k <> "" -> Some (url_decode k, url_decode (String.concat "=" rest))
      | _ -> None)

let split_url_and_query raw_url =
  match String.split_on_char '?' raw_url with
  | [] -> ("/", [])
  | [ p ] -> ((if p = "" then "/" else p), [])
  | p :: rest ->
    let path = if p = "" then "/" else p in
    (path, parse_form_urlencoded (String.concat "?" rest))

(* RFC 9112 Section 7.1 Chunked Transfer-Encoding decoder *)
let decode_chunked_from ~pos (raw : string) : (string * int, [`Need_more | `Error of string]) result =
  let len = String.length raw in
  let buf = Buffer.create 256 in
  let rec read_chunk i =
    match String.index_from_opt raw i '\n' with
    | None -> Error `Need_more
    | Some nl ->
      let line = String.trim (String.sub raw i (nl - i)) in
      let hex_part =
        match String.split_on_char ';' line with hd :: _ -> String.trim hd | [] -> line
      in
      (match int_of_string_opt ("0x" ^ hex_part) with
       | None -> Error (`Error ("Invalid chunk size: " ^ hex_part))
       | Some 0 ->
         let trailer_start = nl + 1 in
         if trailer_start + 1 < len && String.sub raw trailer_start 2 = "\r\n" then
           Ok (Buffer.contents buf, trailer_start + 2)
         else if trailer_start < len && raw.[trailer_start] = '\n' then
           Ok (Buffer.contents buf, trailer_start + 1)
         else Error `Need_more
       | Some chunk_sz ->
         let data_start = nl + 1 in
         if data_start + chunk_sz + 2 > len then Error `Need_more
         else (
           Buffer.add_substring buf raw data_start chunk_sz;
           read_chunk (data_start + chunk_sz + 2)
         ))
  in
  read_chunk pos

let try_parse_request ?(max_header_bytes = 16384) ?(max_body_bytes = 10_485_760) (raw : string) : parse_state =
  match find_crlfcrlf raw with
  | None ->
    if String.length raw > max_header_bytes then
      Malformed "HTTP headers exceed max limit"
    else Need_more_bytes
  | Some hdr_end ->
    if hdr_end > max_header_bytes then Malformed "HTTP headers exceed max limit"
    else
      let hdr_block = String.sub raw 0 hdr_end in
      let lines = String.split_on_char '\n' hdr_block in
      (match lines with
       | [] -> Malformed "Empty HTTP request"
       | req_line :: hdr_lines ->
         let parts = String.split_on_char ' ' (String.trim req_line) in
         let meth, target, version =
           match parts with
           | [ m; t; v ] -> (String.uppercase_ascii m, t, v)
           | [ m; t ] -> (String.uppercase_ascii m, t, "HTTP/1.0")
           | _ -> ("", "", "")
         in
         if meth = "" || target = "" then Malformed "Invalid HTTP request line"
         else
           let headers =
             List.filter_map
               (fun l ->
                 let tl = String.trim l in
                 if tl = "" then None
                 else
                   match String.index_opt tl ':' with
                   | Some idx ->
                     let k = String.trim (String.sub tl 0 idx) in
                     let v = String.trim (String.sub tl (idx + 1) (String.length tl - idx - 1)) in
                     Some (k, v)
                   | None -> None)
               hdr_lines
           in
           let body_start = hdr_end + 4 in
           let te_chunked =
             match header_opt "transfer-encoding" headers with
             | Some v -> String.lowercase_ascii (String.trim v) = "chunked"
             | None -> false
           in
           if te_chunked then
             (match decode_chunked_from ~pos:body_start raw with
              | Error `Need_more -> Need_more_bytes
              | Error (`Error msg) -> Malformed msg
              | Ok (body, next_off) ->
                if String.length body > max_body_bytes then Malformed "Chunked body exceeds max limit"
                else
                  let rem = String.sub raw next_off (String.length raw - next_off) in
                  Complete ({ meth; target; version; headers; body }, rem))
           else
             let cl =
               match header_opt "content-length" headers with
               | None -> 0
               | Some s -> Option.value ~default:(-1) (int_of_string_opt (String.trim s))
             in
             if cl < 0 || cl > max_body_bytes then
               Malformed "Invalid or oversized Content-Length"
             else if String.length raw < body_start + cl then
               Need_more_bytes
             else
               let body = String.sub raw body_start cl in
               let next_off = body_start + cl in
               let rem = String.sub raw next_off (String.length raw - next_off) in
               Complete ({ meth; target; version; headers; body }, rem))

let should_keep_alive (req : parsed_request) : bool =
  match header_opt "connection" req.headers with
  | Some v ->
    let lv = String.lowercase_ascii (String.trim v) in
    if lv = "close" then false
    else if lv = "keep-alive" then true
    else req.version = "HTTP/1.1"
  | None -> req.version = "HTTP/1.1"

let extract_client_ip ~peer_ip headers : string =
  match header_opt "x-forwarded-for" headers with
  | Some xff ->
    (match String.split_on_char ',' xff with
     | first :: _ when String.trim first <> "" -> String.trim first
     | _ -> peer_ip)
  | None ->
    (match header_opt "x-real-ip" headers with
     | Some ip when String.trim ip <> "" -> String.trim ip
     | _ -> peer_ip)

(* HTTP Range header parser + 206 Partial Content slice *)
let parse_range ~total_len (range_hdr : string) : (int * int) option =
  let r = String.lowercase_ascii (String.trim range_hdr) in
  if String.length r > 6 && String.sub r 0 6 = "bytes=" && total_len > 0 then
    let spec = String.sub r 6 (String.length r - 6) in
    match String.split_on_char '-' spec with
    | [ s_str; e_str ] ->
      let s_trim = String.trim s_str and e_trim = String.trim e_str in
      if s_trim = "" && e_trim <> "" then
        (* suffix byte range: bytes=-500 *)
        (match int_of_string_opt e_trim with
         | Some n when n > 0 -> Some (max 0 (total_len - n), total_len - 1)
         | _ -> None)
      else
        (match int_of_string_opt s_trim with
         | Some s when s >= 0 && s < total_len ->
           let e =
             if e_trim = "" then total_len - 1
             else min (total_len - 1) (Option.value ~default:(total_len - 1) (int_of_string_opt e_trim))
           in
           if e >= s then Some (s, e) else None
         | _ -> None)
    | _ -> None
  else None

let format_http_response ~status ~content_type ?(keep_alive = false) ?(headers = []) body : string =
  let has_ct = List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") headers in
  let buf = Buffer.create (String.length body + 256) in
  Printf.bprintf buf "HTTP/1.1 %s\r\n" status;
  if not has_ct then (
    let ct =
      if String.length content_type >= 5 && String.sub content_type 0 5 = "text/" then
        content_type ^ "; charset=utf-8"
      else content_type
    in
    Printf.bprintf buf "Content-Type: %s\r\n" ct
  );
  List.iter
    (fun (k, v) -> Printf.bprintf buf "%s: %s\r\n" k v)
    headers;
  Printf.bprintf buf "Content-Length: %d\r\n" (String.length body);
  Printf.bprintf buf "Connection: %s\r\n\r\n" (if keep_alive then "keep-alive" else "close");
  Buffer.add_string buf body;
  Buffer.contents buf

let mime_of_path path =
  let lower = String.lowercase_ascii path in
  let ext =
    match String.rindex_opt lower '.' with
    | Some idx -> String.sub lower idx (String.length lower - idx)
    | None -> ""
  in
  match ext with
  | ".html" | ".htm" -> "text/html"
  | ".css" -> "text/css"
  | ".js" | ".mjs" -> "application/javascript"
  | ".json" -> "application/json"
  | ".wasm" -> "application/wasm"
  | ".svg" -> "image/svg+xml"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".webp" -> "image/webp"
  | ".gif" -> "image/gif"
  | ".ico" -> "image/x-icon"
  | ".woff2" -> "font/woff2"
  | ".woff" -> "font/woff"
  | ".xml" -> "application/xml"
  | ".txt" | ".md" -> "text/plain"
  | ".mp4" -> "video/mp4"
  | ".pdf" -> "application/pdf"
  | _ -> "application/octet-stream"
