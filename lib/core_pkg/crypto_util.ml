(* Pure-OCaml Cryptographic Primitives & Wire Encodings for Quartz.
   - FIPS 180-4 SHA-256 & RFC 2104 HMAC-SHA256 (signed cookies, tokens)
   - FIPS 180-4 SHA-1 (strictly for RFC 6455 Sec-WebSocket-Accept handshake)
   - RFC 4648 Base64 & Base64url
   - Constant-time string compare *)

let to_hex (s : string) : string =
  let b = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Printf.bprintf b "%02x" (Char.code c)) s;
  Buffer.contents b

let hex_val = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let of_hex (s : string) : string option =
  let n = String.length s in
  if n mod 2 <> 0 then None
  else
    let out = Bytes.create (n / 2) in
    let ok = ref true in
    let i = ref 0 in
    while !ok && !i < n do
      match hex_val s.[!i], hex_val s.[!i + 1] with
      | Some hi, Some lo ->
        Bytes.set out (!i / 2) (Char.chr ((hi lsl 4) lor lo));
        i := !i + 2
      | _ -> ok := false
    done;
    if !ok then Some (Bytes.unsafe_to_string out) else None

let equal_const_time (a : string) (b : string) : bool =
  let la = String.length a and lb = String.length b in
  if la <> lb then false
  else
    let diff = ref 0 in
    for i = 0 to la - 1 do
      diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !diff = 0

(* Pluggable CSPRNG hook: Unix adapter installs /dev/urandom; default uses initialized PRNG *)
let random_bytes_hook : (int -> string) ref =
  let st = Random.State.make [| 0x51756172; 0x747a3430; 0x13579bdf; 0x2468ace0 |] in
  ref (fun n ->
    let b = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set b i (Char.chr (Random.State.int st 256))
    done;
    Bytes.unsafe_to_string b)

let set_random_bytes_provider fn = random_bytes_hook := fn
let random_bytes n = !random_bytes_hook n
let random_hex n_bytes = to_hex (random_bytes n_bytes)

(* RFC 4648 Base64 & Base64url *)
let b64_std = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let b64_url = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let base64_encode_with ?(pad = true) alphabet s =
  let len = String.length s in
  let out_len = ((len + 2) / 3) * 4 in
  let b = Buffer.create out_len in
  let i = ref 0 in
  while !i + 2 < len do
    let v =
      (Char.code s.[!i] lsl 16)
      lor (Char.code s.[!i + 1] lsl 8)
      lor Char.code s.[!i + 2]
    in
    Buffer.add_char b alphabet.[(v lsr 18) land 0x3f];
    Buffer.add_char b alphabet.[(v lsr 12) land 0x3f];
    Buffer.add_char b alphabet.[(v lsr 6) land 0x3f];
    Buffer.add_char b alphabet.[v land 0x3f];
    i := !i + 3
  done;
  let rem = len - !i in
  if rem = 1 then (
    let v = Char.code s.[!i] lsl 16 in
    Buffer.add_char b alphabet.[(v lsr 18) land 0x3f];
    Buffer.add_char b alphabet.[(v lsr 12) land 0x3f];
    if pad then Buffer.add_string b "=="
  ) else if rem = 2 then (
    let v = (Char.code s.[!i] lsl 16) lor (Char.code s.[!i + 1] lsl 8) in
    Buffer.add_char b alphabet.[(v lsr 18) land 0x3f];
    Buffer.add_char b alphabet.[(v lsr 12) land 0x3f];
    Buffer.add_char b alphabet.[(v lsr 6) land 0x3f];
    if pad then Buffer.add_char b '='
  );
  Buffer.contents b

let base64_encode s = base64_encode_with ~pad:true b64_std s
let base64url_encode s = base64_encode_with ~pad:false b64_url s

let b64_char_val c =
  match c with
  | 'A' .. 'Z' -> Some (Char.code c - Char.code 'A')
  | 'a' .. 'z' -> Some (26 + Char.code c - Char.code 'a')
  | '0' .. '9' -> Some (52 + Char.code c - Char.code '0')
  | '+' | '-' -> Some 62
  | '/' | '_' -> Some 63
  | _ -> None

let base64_decode_any s =
  let rec clean_end len =
    if len > 0 && s.[len - 1] = '=' then clean_end (len - 1) else len
  in
  let len = clean_end (String.length s) in
  let b = Buffer.create ((len * 3) / 4) in
  let acc = ref 0 in
  let bits = ref 0 in
  let ok = ref true in
  let i = ref 0 in
  while !ok && !i < len do
    match b64_char_val s.[!i] with
    | None -> ok := false
    | Some v ->
      acc := (!acc lsl 6) lor v;
      bits := !bits + 6;
      if !bits >= 8 then (
        bits := !bits - 8;
        Buffer.add_char b (Char.chr ((!acc lsr !bits) land 0xff))
      );
      incr i
  done;
  if !ok then Some (Buffer.contents b) else None

let base64_decode = base64_decode_any
let base64url_decode = base64_decode_any

(* FIPS 180-4 SHA-256 using Int32 unsigned arithmetic *)
let rotr32 x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let sha256_k =
  [|
    0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl; 0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l; 0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
  |]

let pad_sha_message ~big_endian (msg : string) : bytes =
  let len = String.length msg in
  let bit_len = Int64.mul (Int64.of_int len) 8L in
  let pad_zero_len =
    let rem = (len + 1) mod 64 in
    if rem <= 56 then 56 - rem else 120 - rem
  in
  let total = len + 1 + pad_zero_len + 8 in
  let buf = Bytes.make total '\000' in
  Bytes.blit_string msg 0 buf 0 len;
  Bytes.set buf len '\x80';
  let off = len + 1 + pad_zero_len in
  if big_endian then
    for i = 0 to 7 do
      let shift = (7 - i) * 8 in
      let byte_v = Int64.to_int (Int64.logand (Int64.shift_right_logical bit_len shift) 0xffL) in
      Bytes.set buf (off + i) (Char.chr byte_v)
    done;
  buf

let read_be32 (b : bytes) off =
  let c i = Int32.of_int (Char.code (Bytes.get b (off + i))) in
  Int32.logor
    (Int32.logor (Int32.shift_left (c 0) 24) (Int32.shift_left (c 1) 16))
    (Int32.logor (Int32.shift_left (c 2) 8) (c 3))

let write_be32 (b : bytes) off v =
  Bytes.set b off (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xffl)));
  Bytes.set b (off + 1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xffl)));
  Bytes.set b (off + 2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
  Bytes.set b (off + 3) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))

let sha256_bytes (msg : string) : string =
  let padded = pad_sha_message ~big_endian:true msg in
  let h =
    [|
      0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
      0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
    |]
  in
  let w = Array.make 64 0l in
  let n_blocks = Bytes.length padded / 64 in
  for blk = 0 to n_blocks - 1 do
    let base = blk * 64 in
    for i = 0 to 15 do
      w.(i) <- read_be32 padded (base + (i * 4))
    done;
    for i = 16 to 63 do
      let s0 =
        Int32.logxor (rotr32 w.(i - 15) 7)
          (Int32.logxor (rotr32 w.(i - 15) 18) (Int32.shift_right_logical w.(i - 15) 3))
      in
      let s1 =
        Int32.logxor (rotr32 w.(i - 2) 17)
          (Int32.logxor (rotr32 w.(i - 2) 19) (Int32.shift_right_logical w.(i - 2) 10))
      in
      w.(i) <- Int32.add (Int32.add w.(i - 16) s0) (Int32.add w.(i - 7) s1)
    done;
    let a = ref h.(0) and b = ref h.(1) and c = ref h.(2) and d = ref h.(3) in
    let e = ref h.(4) and f = ref h.(5) and g = ref h.(6) and hh = ref h.(7) in
    for i = 0 to 63 do
      let s1 =
        Int32.logxor (rotr32 !e 6) (Int32.logxor (rotr32 !e 11) (rotr32 !e 25))
      in
      let ch = Int32.logxor (Int32.logand !e !f) (Int32.logand (Int32.lognot !e) !g) in
      let temp1 =
        Int32.add !hh (Int32.add s1 (Int32.add ch (Int32.add sha256_k.(i) w.(i))))
      in
      let s0 =
        Int32.logxor (rotr32 !a 2) (Int32.logxor (rotr32 !a 13) (rotr32 !a 22))
      in
      let maj =
        Int32.logxor (Int32.logand !a !b)
          (Int32.logxor (Int32.logand !a !c) (Int32.logand !b !c))
      in
      let temp2 = Int32.add s0 maj in
      hh := !g;
      g := !f;
      f := !e;
      e := Int32.add !d temp1;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add temp1 temp2
    done;
    h.(0) <- Int32.add h.(0) !a;
    h.(1) <- Int32.add h.(1) !b;
    h.(2) <- Int32.add h.(2) !c;
    h.(3) <- Int32.add h.(3) !d;
    h.(4) <- Int32.add h.(4) !e;
    h.(5) <- Int32.add h.(5) !f;
    h.(6) <- Int32.add h.(6) !g;
    h.(7) <- Int32.add h.(7) !hh
  done;
  let out = Bytes.create 32 in
  for i = 0 to 7 do
    write_be32 out (i * 4) h.(i)
  done;
  Bytes.unsafe_to_string out

let sha256_hex msg = to_hex (sha256_bytes msg)

(* RFC 2104 HMAC-SHA256 *)
let hmac_sha256_bytes ~secret (msg : string) : string =
  let key0 =
    if String.length secret > 64 then sha256_bytes secret else secret
  in
  let key = Bytes.make 64 '\000' in
  Bytes.blit_string key0 0 key 0 (String.length key0);
  let o_key = Bytes.create 64 in
  let i_key = Bytes.create 64 in
  for i = 0 to 63 do
    let kv = Char.code (Bytes.get key i) in
    Bytes.set o_key i (Char.chr (kv lxor 0x5c));
    Bytes.set i_key i (Char.chr (kv lxor 0x36))
  done;
  let inner = sha256_bytes (Bytes.unsafe_to_string i_key ^ msg) in
  sha256_bytes (Bytes.unsafe_to_string o_key ^ inner)

let hmac_sha256_hex ~secret msg = to_hex (hmac_sha256_bytes ~secret msg)

module Hmac_signer : Io.SIGNER = struct
  let sign ~secret ~payload = hmac_sha256_hex ~secret payload
  let verify ~secret ~payload ~sig_hex =
    let expected = hmac_sha256_hex ~secret payload in
    equal_const_time expected sig_hex
end

(* FIPS 180-4 SHA-1 -- used strictly for RFC 6455 Sec-WebSocket-Accept handshake *)
let rotl32 x n =
  Int32.logor (Int32.shift_left x n) (Int32.shift_right_logical x (32 - n))

let sha1_bytes (msg : string) : string =
  let padded = pad_sha_message ~big_endian:true msg in
  let h = [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l; 0xc3d2e1f0l |] in
  let w = Array.make 80 0l in
  let n_blocks = Bytes.length padded / 64 in
  for blk = 0 to n_blocks - 1 do
    let base = blk * 64 in
    for i = 0 to 15 do
      w.(i) <- read_be32 padded (base + (i * 4))
    done;
    for i = 16 to 79 do
      w.(i) <-
        rotl32
          (Int32.logxor w.(i - 3)
             (Int32.logxor w.(i - 8) (Int32.logxor w.(i - 14) w.(i - 16))))
          1
    done;
    let a = ref h.(0) and b = ref h.(1) and c = ref h.(2) and d = ref h.(3) and e = ref h.(4) in
    for i = 0 to 79 do
      let f, k =
        if i < 20 then
          (Int32.logor (Int32.logand !b !c) (Int32.logand (Int32.lognot !b) !d), 0x5a827999l)
        else if i < 40 then
          (Int32.logxor !b (Int32.logxor !c !d), 0x6ed9eba1l)
        else if i < 60 then
          ( Int32.logor (Int32.logand !b !c)
              (Int32.logor (Int32.logand !b !d) (Int32.logand !c !d)),
            0x8f1bbcdcl )
        else (Int32.logxor !b (Int32.logxor !c !d), 0xca62c1d6l)
      in
      let temp = Int32.add (rotl32 !a 5) (Int32.add f (Int32.add !e (Int32.add k w.(i)))) in
      e := !d;
      d := !c;
      c := rotl32 !b 30;
      b := !a;
      a := temp
    done;
    h.(0) <- Int32.add h.(0) !a;
    h.(1) <- Int32.add h.(1) !b;
    h.(2) <- Int32.add h.(2) !c;
    h.(3) <- Int32.add h.(3) !d;
    h.(4) <- Int32.add h.(4) !e
  done;
  let out = Bytes.create 20 in
  for i = 0 to 4 do
    write_be32 out (i * 4) h.(i)
  done;
  Bytes.unsafe_to_string out

(* RFC 2898 / RFC 6070 PBKDF2-HMAC-SHA256 for password hashing (zero Unix dependency) *)
let xor_bytes_in_place dst src len =
  for i = 0 to len - 1 do
    Bytes.set dst i
      (Char.chr ((Char.code (Bytes.get dst i)) lxor (Char.code (String.get src i))))
  done

let pbkdf2_sha256 ~(password : string) ~(salt : string) ~(iterations : int) ~(dk_len : int) : string =
  let iter = max 1 iterations in
  let h_len = 32 in
  let blocks = (dk_len + h_len - 1) / h_len in
  let out = Buffer.create dk_len in
  for blk = 1 to blocks do
    let blk_buf = Bytes.create (String.length salt + 4) in
    Bytes.blit_string salt 0 blk_buf 0 (String.length salt);
    write_be32 blk_buf (String.length salt) (Int32.of_int blk);
    let u = ref (hmac_sha256_bytes ~secret:password (Bytes.unsafe_to_string blk_buf)) in
    let t = Bytes.of_string !u in
    for _ = 2 to iter do
      let next_u = hmac_sha256_bytes ~secret:password !u in
      xor_bytes_in_place t next_u h_len;
      u := next_u
    done;
    Buffer.add_bytes out t
  done;
  String.sub (Buffer.contents out) 0 dk_len

let from_hex_opt (hex : string) : string option =
  let len = String.length hex in
  if len mod 2 <> 0 then None
  else
    let n = len / 2 in
    let out = Bytes.create n in
    let hex_val c =
      match c with
      | '0' .. '9' -> Some (Char.code c - Char.code '0')
      | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
      | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
      | _ -> None
    in
    let rec loop i =
      if i = n then Some (Bytes.unsafe_to_string out)
      else
        match (hex_val hex.[i * 2], hex_val hex.[(i * 2) + 1]) with
        | Some hi, Some lo ->
          Bytes.set out i (Char.chr ((hi lsl 4) lor lo));
          loop (i + 1)
        | _ -> None
    in
    loop 0

let hash_password ?(iterations = 10_000) (password : string) : string =
  let iter = max 1 iterations in
  let salt = random_bytes 16 in
  let dk = pbkdf2_sha256 ~password ~salt ~iterations:iter ~dk_len:32 in
  Printf.sprintf "pbkdf2-sha256$%d$%s$%s" iter (to_hex salt) (to_hex dk)

let verify_password ~(password : string) ~(stored_hash : string) : bool =
  match String.split_on_char '$' stored_hash with
  | [ "pbkdf2-sha256"; iter_s; salt_hex; expected_hex ] ->
    (match (int_of_string_opt iter_s, from_hex_opt salt_hex) with
     | Some iter, Some salt when iter > 0 ->
       let dk = pbkdf2_sha256 ~password ~salt ~iterations:iter ~dk_len:32 in
       equal_const_time expected_hex (to_hex dk)
     | _ -> false)
  | _ -> false

module Pbkdf2_hasher : Io.PASSWORD_HASHER = struct
  let hash_password = hash_password
  let verify_password = verify_password
end
