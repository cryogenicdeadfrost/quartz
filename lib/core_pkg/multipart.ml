(* Pure-OCaml RFC 7578 multipart/form-data Parser + Magic-Byte MIME Sniffer.
   Zero Unix dependency: runs safely inside servers, Edge workers, and unikernels. *)

type part = {
  field_name : string;
  filename : string option;
  content_type : string;
  sniffed_mime : string;
  data : string;
}

let starts_with_bytes ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let sniff_mime (data : string) : string =
  if starts_with_bytes ~prefix:"\x89PNG\r\n\x1a\n" data then "image/png"
  else if starts_with_bytes ~prefix:"\xff\xd8\xff" data then "image/jpeg"
  else if starts_with_bytes ~prefix:"GIF87a" data || starts_with_bytes ~prefix:"GIF89a" data then "image/gif"
  else if String.length data >= 12
          && String.sub data 0 4 = "RIFF"
          && String.sub data 8 4 = "WEBP"
  then "image/webp"
  else if starts_with_bytes ~prefix:"%PDF-" data then "application/pdf"
  else if starts_with_bytes ~prefix:"PK\x03\x04" data then "application/zip"
  else if starts_with_bytes ~prefix:"<svg" (String.trim data) then "image/svg+xml"
  else if I18n.utf8_valid data then "text/plain; charset=utf-8"
  else "application/octet-stream"

let extract_boundary (content_type : string) : string option =
  let parts = String.split_on_char ';' content_type in
  List.find_map
    (fun p ->
      let t = String.trim p in
      let low = String.lowercase_ascii t in
      if String.length low > 9 && String.sub low 0 9 = "boundary=" then
        let raw = String.trim (String.sub t 9 (String.length t - 9)) in
        let clean =
          if String.length raw >= 2 && raw.[0] = '"' && raw.[String.length raw - 1] = '"' then
            String.sub raw 1 (String.length raw - 2)
          else raw
        in
        if clean = "" then None else Some clean
      else None)
    parts

let find_sub ~needle ~start haystack =
  let nlen = String.length needle and hlen = String.length haystack in
  if nlen = 0 then Some start
  else
    let rec loop i =
      if i + nlen > hlen then None
      else if String.sub haystack i nlen = needle then Some i
      else loop (i + 1)
    in
    loop start

let extract_param key header_val =
  let k_eq = key ^ "=\"" in
  match find_sub ~needle:k_eq ~start:0 header_val with
  | None -> None
  | Some idx ->
    let vstart = idx + String.length k_eq in
    (match String.index_from_opt header_val vstart '"' with
     | Some vend -> Some (String.sub header_val vstart (vend - vstart))
     | None -> None)

let parse ?(max_parts = 32) ?(max_part_bytes = 5_242_880) ~content_type (body : string)
    : (part list, string) result =
  match extract_boundary content_type with
  | None -> Error "Missing multipart boundary in Content-Type"
  | Some boundary ->
    let delim = "--" ^ boundary in
    let dlen = String.length delim in
    let rec split_parts pos acc count =
      if count > max_parts then Error "Too many multipart fields"
      else
        match find_sub ~needle:delim ~start:pos body with
        | None -> Ok (List.rev acc)
        | Some idx ->
          let after = idx + dlen in
          if after + 1 < String.length body && String.sub body after 2 = "--" then
            Ok (List.rev acc)
          else
            let start_hdr =
              if after + 1 < String.length body && String.sub body after 2 = "\r\n" then after + 2
              else if after < String.length body && body.[after] = '\n' then after + 1
              else after
            in
            (match find_sub ~needle:"\r\n\r\n" ~start:start_hdr body with
             | None -> Ok (List.rev acc)
             | Some hdr_end ->
               let hdr_str = String.sub body start_hdr (hdr_end - start_hdr) in
               let data_start = hdr_end + 4 in
               (match find_sub ~needle:delim ~start:data_start body with
                | None -> Error "Unterminated multipart boundary"
                | Some next_delim ->
                  let data_end =
                    if next_delim >= 2 && String.sub body (next_delim - 2) 2 = "\r\n" then next_delim - 2
                    else if next_delim >= 1 && body.[next_delim - 1] = '\n' then next_delim - 1
                    else next_delim
                  in
                  let dlen = max 0 (data_end - data_start) in
                  if dlen > max_part_bytes then
                    Error (Printf.sprintf "Multipart field exceeds max %d bytes" max_part_bytes)
                  else
                    let data = String.sub body data_start dlen in
                    let hdr_lines = String.split_on_char '\n' hdr_str in
                    let field_name = ref "" in
                    let filename = ref None in
                    let p_ct = ref "application/octet-stream" in
                    List.iter
                      (fun l ->
                        let tl = String.trim l in
                        let low = String.lowercase_ascii tl in
                        if starts_with_bytes ~prefix:"content-disposition:" low then (
                          (match extract_param "name" tl with Some n -> field_name := n | None -> ());
                          filename := extract_param "filename" tl
                        ) else if starts_with_bytes ~prefix:"content-type:" low then
                          match String.index_opt tl ':' with
                          | Some ci ->
                            p_ct := String.trim (String.sub tl (ci + 1) (String.length tl - ci - 1))
                          | None -> ())
                      hdr_lines;
                    let part =
                      {
                        field_name = !field_name;
                        filename = !filename;
                        content_type = !p_ct;
                        sniffed_mime = sniff_mime data;
                        data;
                      }
                    in
                    split_parts next_delim (part :: acc) (count + 1)))
    in
    split_parts 0 [] 0
