(* Pure-OCaml RFC 8259 JSON parser, serializer, and typed Decode/Encode combinators.
   Zero third-party dependencies -- runs on native, MirageOS, js_of_ocaml, and Wasm. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

let null = Null
let bool b = Bool b
let int i = Int i
let float f = Float f
let string s = String s
let array xs = Array xs
let obj kvs = Object kvs

let encode_string_into buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
        Printf.bprintf buf "\\u%04x" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let rec write_compact buf = function
  | Null -> Buffer.add_string buf "null"
  | Bool true -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | Int i -> Buffer.add_string buf (string_of_int i)
  | Float f ->
    if Float.is_nan f || Float.is_infinite f then Buffer.add_string buf "null"
    else
      let s = Printf.sprintf "%.15g" f in
      Buffer.add_string buf s
  | String s -> encode_string_into buf s
  | Array xs ->
    Buffer.add_char buf '[';
    List.iteri
      (fun i x ->
        if i > 0 then Buffer.add_char buf ',';
        write_compact buf x)
      xs;
    Buffer.add_char buf ']'
  | Object kvs ->
    Buffer.add_char buf '{';
    List.iteri
      (fun i (k, v) ->
        if i > 0 then Buffer.add_char buf ',';
        encode_string_into buf k;
        Buffer.add_char buf ':';
        write_compact buf v)
      kvs;
    Buffer.add_char buf '}'

let to_string v =
  let b = Buffer.create 128 in
  write_compact b v;
  Buffer.contents b

let to_pretty_string ?(indent = 2) v =
  let b = Buffer.create 256 in
  let pad depth = String.make (depth * indent) ' ' in
  let rec write depth = function
    | (Null | Bool _ | Int _ | Float _ | String _) as prim -> write_compact b prim
    | Array [] -> Buffer.add_string b "[]"
    | Array xs ->
      Buffer.add_string b "[\n";
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string b ",\n";
          Buffer.add_string b (pad (depth + 1));
          write (depth + 1) x)
        xs;
      Buffer.add_char b '\n';
      Buffer.add_string b (pad depth);
      Buffer.add_char b ']'
    | Object [] -> Buffer.add_string b "{}"
    | Object kvs ->
      Buffer.add_string b "{\n";
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_string b ",\n";
          Buffer.add_string b (pad (depth + 1));
          encode_string_into b k;
          Buffer.add_string b ": ";
          write (depth + 1) v)
        kvs;
      Buffer.add_char b '\n';
      Buffer.add_string b (pad depth);
      Buffer.add_char b '}'
  in
  write 0 v;
  Buffer.contents b

(* Parser *)
type parser_state = {
  src : string;
  len : int;
  mutable pos : int;
}

let skip_ws st =
  while
    st.pos < st.len
    &&
    match st.src.[st.pos] with
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  do
    st.pos <- st.pos + 1
  done

let parse_error st msg =
  Error (Printf.sprintf "JSON parse error at offset %d: %s" st.pos msg)

let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let read_hex4 st =
  if st.pos + 4 > st.len then None
  else
    match
      ( hex_digit st.src.[st.pos],
        hex_digit st.src.[st.pos + 1],
        hex_digit st.src.[st.pos + 2],
        hex_digit st.src.[st.pos + 3] )
    with
    | Some a, Some b, Some c, Some d ->
      st.pos <- st.pos + 4;
      Some ((a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d)
    | _ -> None

let add_utf8_codepoint buf cp =
  if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
  else if cp <= 0x7FF then (
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  ) else if cp <= 0xFFFF then (
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  ) else (
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  )

let parse_string st =
  if st.pos >= st.len || st.src.[st.pos] <> '"' then
    parse_error st "expected '\"'"
  else (
    st.pos <- st.pos + 1;
    let buf = Buffer.create 32 in
    let rec loop () =
      if st.pos >= st.len then parse_error st "unterminated string"
      else
        let c = st.src.[st.pos] in
        if c = '"' then (
          st.pos <- st.pos + 1;
          Ok (Buffer.contents buf)
        ) else if c = '\\' then (
          st.pos <- st.pos + 1;
          if st.pos >= st.len then parse_error st "unterminated escape"
          else
            let esc = st.src.[st.pos] in
            st.pos <- st.pos + 1;
            match esc with
            | '"' -> Buffer.add_char buf '"'; loop ()
            | '\\' -> Buffer.add_char buf '\\'; loop ()
            | '/' -> Buffer.add_char buf '/'; loop ()
            | 'b' -> Buffer.add_char buf '\b'; loop ()
            | 'f' -> Buffer.add_char buf '\012'; loop ()
            | 'n' -> Buffer.add_char buf '\n'; loop ()
            | 'r' -> Buffer.add_char buf '\r'; loop ()
            | 't' -> Buffer.add_char buf '\t'; loop ()
            | 'u' ->
              (match read_hex4 st with
               | None -> parse_error st "invalid \\uXXXX escape"
               | Some hi ->
                 if hi >= 0xD800 && hi <= 0xDBFF
                    && st.pos + 2 <= st.len
                    && st.src.[st.pos] = '\\'
                    && st.src.[st.pos + 1] = 'u'
                 then (
                   st.pos <- st.pos + 2;
                   match read_hex4 st with
                   | Some lo when lo >= 0xDC00 && lo <= 0xDFFF ->
                     let cp = 0x10000 + (((hi - 0xD800) lsl 10) lor (lo - 0xDC00)) in
                     add_utf8_codepoint buf cp;
                     loop ()
                   | _ -> parse_error st "invalid UTF-16 surrogate pair"
                 ) else (
                   add_utf8_codepoint buf hi;
                   loop ()
                 ))
            | other -> parse_error st (Printf.sprintf "unknown escape '\\%c'" other)
        ) else if Char.code c < 0x20 then
          parse_error st "unescaped control character in string"
        else (
          Buffer.add_char buf c;
          st.pos <- st.pos + 1;
          loop ()
        )
    in
    loop ()
  )

let parse_number st =
  let start = st.pos in
  let is_float = ref false in
  if st.pos < st.len && st.src.[st.pos] = '-' then st.pos <- st.pos + 1;
  let digit_start = st.pos in
  while st.pos < st.len && st.src.[st.pos] >= '0' && st.src.[st.pos] <= '9' do
    st.pos <- st.pos + 1
  done;
  if st.pos = digit_start then parse_error st "invalid number"
  else (
    if st.pos < st.len && st.src.[st.pos] = '.' then (
      is_float := true;
      st.pos <- st.pos + 1;
      let frac_start = st.pos in
      while st.pos < st.len && st.src.[st.pos] >= '0' && st.src.[st.pos] <= '9' do
        st.pos <- st.pos + 1
      done;
      if st.pos = frac_start then st.pos <- st.pos (* handled below *)
    );
    if st.pos < st.len && (st.src.[st.pos] = 'e' || st.src.[st.pos] = 'E') then (
      is_float := true;
      st.pos <- st.pos + 1;
      if st.pos < st.len && (st.src.[st.pos] = '+' || st.src.[st.pos] = '-') then
        st.pos <- st.pos + 1;
      let exp_start = st.pos in
      while st.pos < st.len && st.src.[st.pos] >= '0' && st.src.[st.pos] <= '9' do
        st.pos <- st.pos + 1
      done;
      if st.pos = exp_start then st.pos <- start
    );
    if st.pos <= start then parse_error st "malformed numeric literal"
    else
      let raw = String.sub st.src start (st.pos - start) in
      if !is_float then
        match float_of_string_opt raw with
        | Some f -> Ok (Float f)
        | None -> parse_error st ("invalid float: " ^ raw)
      else
        match int_of_string_opt raw with
        | Some i -> Ok (Int i)
        | None ->
          (match float_of_string_opt raw with
           | Some f -> Ok (Float f)
           | None -> parse_error st ("invalid number: " ^ raw))
  )

let match_literal st lit res =
  let n = String.length lit in
  if st.pos + n <= st.len && String.sub st.src st.pos n = lit then (
    st.pos <- st.pos + n;
    Ok res
  ) else parse_error st ("expected " ^ lit)

let rec parse_value st =
  skip_ws st;
  if st.pos >= st.len then parse_error st "unexpected end of input"
  else
    match st.src.[st.pos] with
    | 'n' -> match_literal st "null" Null
    | 't' -> match_literal st "true" (Bool true)
    | 'f' -> match_literal st "false" (Bool false)
    | '"' ->
      (match parse_string st with
       | Ok s -> Ok (String s)
       | Error _ as e -> e)
    | '[' -> parse_array st
    | '{' -> parse_object st
    | '0' .. '9' | '-' -> parse_number st
    | c -> parse_error st (Printf.sprintf "unexpected character '%c'" c)

and parse_array st =
  st.pos <- st.pos + 1;
  skip_ws st;
  if st.pos < st.len && st.src.[st.pos] = ']' then (
    st.pos <- st.pos + 1;
    Ok (Array [])
  ) else
    let rec loop acc =
      match parse_value st with
      | Error _ as e -> e
      | Ok v ->
        skip_ws st;
        if st.pos >= st.len then parse_error st "unterminated array"
        else if st.src.[st.pos] = ',' then (
          st.pos <- st.pos + 1;
          loop (v :: acc)
        ) else if st.src.[st.pos] = ']' then (
          st.pos <- st.pos + 1;
          Ok (Array (List.rev (v :: acc)))
        ) else parse_error st "expected ',' or ']' in array"
    in
    loop []

and parse_object st =
  st.pos <- st.pos + 1;
  skip_ws st;
  if st.pos < st.len && st.src.[st.pos] = '}' then (
    st.pos <- st.pos + 1;
    Ok (Object [])
  ) else
    let rec loop acc =
      skip_ws st;
      match parse_string st with
      | Error _ as e -> e
      | Ok key ->
        skip_ws st;
        if st.pos >= st.len || st.src.[st.pos] <> ':' then
          parse_error st "expected ':' after object key"
        else (
          st.pos <- st.pos + 1;
          match parse_value st with
          | Error _ as e -> e
          | Ok v ->
            skip_ws st;
            if st.pos >= st.len then parse_error st "unterminated object"
            else if st.src.[st.pos] = ',' then (
              st.pos <- st.pos + 1;
              loop ((key, v) :: acc)
            ) else if st.src.[st.pos] = '}' then (
              st.pos <- st.pos + 1;
              Ok (Object (List.rev ((key, v) :: acc)))
            ) else parse_error st "expected ',' or '}' in object"
        )
    in
    loop []

let parse src =
  let st = { src; len = String.length src; pos = 0 } in
  match parse_value st with
  | Error _ as e -> e
  | Ok v ->
    skip_ws st;
    if st.pos < st.len then parse_error st "trailing characters after JSON value"
    else Ok v

let parse_opt src =
  match parse src with Ok v -> Some v | Error _ -> None

(* Accessors *)
let get_field k = function
  | Object kvs -> List.assoc_opt k kvs
  | _ -> None

let get_string = function String s -> Some s | _ -> None
let get_int = function Int i -> Some i | Float f -> Some (int_of_float f) | _ -> None
let get_float = function Float f -> Some f | Int i -> Some (float_of_int i) | _ -> None
let get_bool = function Bool b -> Some b | _ -> None
let get_list = function Array xs -> Some xs | _ -> None

(* Typed Zod-style Decode Combinators with JSONPath diagnostics *)
module Decode = struct
  type 'a decoder = string list -> t -> ('a, string) result

  let path_str p =
    match List.rev p with
    | [] -> "$"
    | segs -> "$." ^ String.concat "." segs

  let fail p msg = Error (Printf.sprintf "At %s: %s" (path_str p) msg)

  let kind_name = function
    | Null -> "null"
    | Bool _ -> "bool"
    | Int _ -> "int"
    | Float _ -> "float"
    | String _ -> "string"
    | Array _ -> "array"
    | Object _ -> "object"

  let string p = function
    | String s -> Ok s
    | v -> fail p ("expected string, got " ^ kind_name v)

  let int p = function
    | Int i -> Ok i
    | v -> fail p ("expected int, got " ^ kind_name v)

  let float p = function
    | Float f -> Ok f
    | Int i -> Ok (float_of_int i)
    | v -> fail p ("expected float, got " ^ kind_name v)

  let bool p = function
    | Bool b -> Ok b
    | v -> fail p ("expected bool, got " ^ kind_name v)

  let null_or d p = function
    | Null -> Ok None
    | v -> Result.map (fun x -> Some x) (d p v)

  let list d p = function
    | Array xs ->
      let rec go i acc = function
        | [] -> Ok (List.rev acc)
        | x :: rest ->
          (match d (Printf.sprintf "[%d]" i :: p) x with
           | Ok v -> go (i + 1) (v :: acc) rest
           | Error _ as e -> e)
      in
      go 0 [] xs
    | v -> fail p ("expected array, got " ^ kind_name v)

  let field name d p = function
    | Object kvs ->
      (match List.assoc_opt name kvs with
       | None -> fail p (Printf.sprintf "missing required field '%s'" name)
       | Some sub -> d (name :: p) sub)
    | v -> fail p ("expected object, got " ^ kind_name v)

  let optional_field name d p = function
    | Object kvs ->
      (match List.assoc_opt name kvs with
       | None | Some Null -> Ok None
       | Some sub -> Result.map (fun x -> Some x) (d (name :: p) sub))
    | v -> fail p ("expected object, got " ^ kind_name v)

  let map f d p v =
    match d p v with
    | Ok a -> Ok (f a)
    | Error _ as e -> e

  let bind d f p v =
    match d p v with
    | Ok a -> f a p v
    | Error _ as e -> e

  let both d1 d2 p v =
    match d1 p v with
    | Error _ as e -> e
    | Ok a ->
      (match d2 p v with
       | Error _ as e -> e
       | Ok b -> Ok (a, b))

  let run d v = d [] v

  let decode_string d raw =
    match parse raw with
    | Error e -> Error e
    | Ok v -> run d v
end

module Encode = struct
  let string s = String s
  let int i = Int i
  let float f = Float f
  let bool b = Bool b
  let null = Null
  let list f xs = Array (List.map f xs)
  let option f = function None -> Null | Some v -> f v
  let obj kvs = Object kvs
end
