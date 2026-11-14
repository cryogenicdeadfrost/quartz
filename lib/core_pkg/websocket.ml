(* Pure-OCaml RFC 6455 WebSocket Wire Protocol & Topic Rooms (`Quartz.Websocket`).
   Implements:
   - HTTP 101 Switching Protocols upgrade + SHA-1 Sec-WebSocket-Accept key derivation
   - Full binary/text/ping/pong/close frame codec (masking + 7/16/64-bit lengths)
   - Room-based fan-out messaging (`Ws_rooms`) with zero external dependencies *)

let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_key ~(sec_websocket_key : string) : string =
  let combined = String.trim sec_websocket_key ^ websocket_guid in
  Crypto_util.base64_encode (Crypto_util.sha1_bytes combined)

let upgrade_response ~(sec_websocket_key : string) : string =
  let acc = accept_key ~sec_websocket_key in
  Printf.sprintf
    "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n"
    acc

type opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  | Reserved of int

let opcode_of_int = function
  | 0x0 -> Continuation
  | 0x1 -> Text
  | 0x2 -> Binary
  | 0x8 -> Close
  | 0x9 -> Ping
  | 0xA -> Pong
  | n -> Reserved n

let int_of_opcode = function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xA
  | Reserved n -> n land 0x0F

type frame = {
  fin : bool;
  opcode : opcode;
  payload : string;
}

let encode_frame ?mask (f : frame) : string =
  let b0 = (if f.fin then 0x80 else 0x00) lor (int_of_opcode f.opcode land 0x0F) in
  let len = String.length f.payload in
  let mask_bit = match mask with Some _ -> 0x80 | None -> 0x00 in
  let buf = Buffer.create (len + 16) in
  Buffer.add_char buf (Char.chr b0);
  if len <= 125 then
    Buffer.add_char buf (Char.chr (mask_bit lor len))
  else if len <= 65535 then (
    Buffer.add_char buf (Char.chr (mask_bit lor 126));
    Buffer.add_char buf (Char.chr ((len lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr (len land 0xFF))
  ) else (
    Buffer.add_char buf (Char.chr (mask_bit lor 127));
    let len64 = Int64.of_int len in
    for i = 7 downto 0 do
      let v = Int64.to_int (Int64.logand (Int64.shift_right_logical len64 (i * 8)) 0xFFL) in
      Buffer.add_char buf (Char.chr v)
    done
  );
  match mask with
  | None ->
    Buffer.add_string buf f.payload;
    Buffer.contents buf
  | Some m ->
    if String.length m < 4 then invalid_arg "Websocket.encode_frame: mask must be 4 bytes";
    Buffer.add_string buf (String.sub m 0 4);
    for i = 0 to len - 1 do
      let masked_c = Char.chr (Char.code f.payload.[i] lxor Char.code m.[i mod 4]) in
      Buffer.add_char buf masked_c
    done;
    Buffer.contents buf

let text_frame ?(fin = true) payload = encode_frame { fin; opcode = Text; payload }
let binary_frame ?(fin = true) payload = encode_frame { fin; opcode = Binary; payload }
let pong_frame payload = encode_frame { fin = true; opcode = Pong; payload }
let close_frame ?(code = 1000) () =
  let p = Bytes.create 2 in
  Bytes.set p 0 (Char.chr ((code lsr 8) land 0xFF));
  Bytes.set p 1 (Char.chr (code land 0xFF));
  encode_frame { fin = true; opcode = Close; payload = Bytes.unsafe_to_string p }

type decode_status =
  | Frame of frame * string (* decoded frame + remaining buffer *)
  | Need_more
  | Protocol_error of string

let decode_frame (raw : string) : decode_status =
  let total = String.length raw in
  if total < 2 then Need_more
  else
    let b0 = Char.code raw.[0] in
    let b1 = Char.code raw.[1] in
    let fin = (b0 land 0x80) <> 0 in
    let opcode = opcode_of_int (b0 land 0x0F) in
    let masked = (b1 land 0x80) <> 0 in
    let len7 = b1 land 0x7F in
    let header_len, payload_len_opt =
      if len7 < 126 then (2, Some len7)
      else if len7 = 126 then
        if total < 4 then (4, None)
        else
          let l = (Char.code raw.[2] lsl 8) lor Char.code raw.[3] in
          (4, Some l)
      else
        if total < 10 then (10, None)
        else
          let acc = ref 0L in
          for i = 2 to 9 do
            acc := Int64.logor (Int64.shift_left !acc 8) (Int64.of_int (Char.code raw.[i]))
          done;
          if !acc > Int64.of_int Sys.max_string_length then (10, Some (-1))
          else (10, Some (Int64.to_int !acc))
    in
    match payload_len_opt with
    | None -> Need_more
    | Some plen when plen < 0 -> Protocol_error "Frame exceeds OCaml max string length"
    | Some plen ->
      let mask_len = if masked then 4 else 0 in
      let frame_total = header_len + mask_len + plen in
      if total < frame_total then Need_more
      else
        let payload_off = header_len + mask_len in
        let payload =
          if not masked then
            String.sub raw payload_off plen
          else (
            let out = Bytes.create plen in
            for i = 0 to plen - 1 do
              let m = Char.code raw.[header_len + (i mod 4)] in
              let c = Char.code raw.[payload_off + i] in
              Bytes.set out i (Char.chr (c lxor m))
            done;
            Bytes.unsafe_to_string out
          )
        in
        let rest = String.sub raw frame_total (total - frame_total) in
        Frame ({ fin; opcode; payload }, rest)

(* Topic Room Registry *)
module Rooms = struct
  type sender = string -> unit

  let rooms : (string, (int * sender) list) Hashtbl.t = Hashtbl.create 16
  let next_conn_id = ref 1

  let new_conn_id () =
    let id = !next_conn_id in
    incr next_conn_id;
    id

  let join ~room ~conn_id (send_fn : sender) =
    let cur = Option.value ~default:[] (Hashtbl.find_opt rooms room) in
    let filtered = List.filter (fun (cid, _) -> cid <> conn_id) cur in
    Hashtbl.replace rooms room ((conn_id, send_fn) :: filtered)

  let leave ~room ~conn_id =
    match Hashtbl.find_opt rooms room with
    | None -> ()
    | Some lst ->
      Hashtbl.replace rooms room (List.filter (fun (cid, _) -> cid <> conn_id) lst)

  let leave_all ~conn_id =
    Hashtbl.iter
      (fun r lst -> Hashtbl.replace rooms r (List.filter (fun (cid, _) -> cid <> conn_id) lst))
      rooms

  let room_size room =
    match Hashtbl.find_opt rooms room with None -> 0 | Some l -> List.length l

  let broadcast ~room ?except_conn_id msg =
    let wire = text_frame msg in
    match Hashtbl.find_opt rooms room with
    | None -> 0
    | Some lst ->
      let sent = ref 0 in
      List.iter
        (fun (cid, send_fn) ->
          if except_conn_id <> Some cid then (
            (try send_fn wire with _ -> ());
            incr sent
          ))
        lst;
      !sent
end
