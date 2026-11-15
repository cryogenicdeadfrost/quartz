(* Pure-OCaml Unicode UTF-8 Codec, CLDR Plural Rules (20+ locales),
   Message Catalog Interpolation, and UTC Calendar / RFC 7231 Date Math.
   Zero external dependencies -- identical behavior across Unix, MirageOS, and Edge. *)

(* 1. Pure UTF-8 Decoder / Validator *)
let utf8_decode_step (s : string) (i : int) : (int * int) option =
  let len = String.length s in
  if i >= len then None
  else
    let b0 = Char.code s.[i] in
    if b0 < 0x80 then Some (b0, 1)
    else if b0 >= 0xC2 && b0 <= 0xDF then (
      if i + 1 >= len then None
      else
        let b1 = Char.code s.[i + 1] in
        if b1 land 0xC0 <> 0x80 then None
        else Some (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 2)
    ) else if b0 >= 0xE0 && b0 <= 0xEF then (
      if i + 2 >= len then None
      else
        let b1 = Char.code s.[i + 1] and b2 = Char.code s.[i + 2] in
        if b1 land 0xC0 <> 0x80 || b2 land 0xC0 <> 0x80 then None
        else if b0 = 0xE0 && b1 < 0xA0 then None
        else if b0 = 0xED && b1 >= 0xA0 then None
        else
          Some
            ( ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F),
              3 )
    ) else if b0 >= 0xF0 && b0 <= 0xF4 then (
      if i + 3 >= len then None
      else
        let b1 = Char.code s.[i + 1]
        and b2 = Char.code s.[i + 2]
        and b3 = Char.code s.[i + 3] in
        if b1 land 0xC0 <> 0x80 || b2 land 0xC0 <> 0x80 || b3 land 0xC0 <> 0x80 then
          None
        else if b0 = 0xF0 && b1 < 0x90 then None
        else if b0 = 0xF4 && b1 > 0x8F then None
        else
          Some
            ( ((b0 land 0x07) lsl 18)
              lor ((b1 land 0x3F) lsl 12)
              lor ((b2 land 0x3F) lsl 6)
              lor (b3 land 0x3F),
              4 )
    ) else None

let utf8_valid s =
  let len = String.length s in
  let rec go i =
    if i = len then true
    else
      match utf8_decode_step s i with
      | None -> false
      | Some (_, step) -> go (i + step)
  in
  go 0

let utf8_length s =
  let len = String.length s in
  let rec go i count =
    if i >= len then count
    else
      match utf8_decode_step s i with
      | None -> go (i + 1) (count + 1)
      | Some (_, step) -> go (i + step) (count + 1)
  in
  go 0 0

(* 2. CLDR Plural Categories *)
type plural =
  | Zero
  | One
  | Two
  | Few
  | Many
  | Other

let plural_suffix = function
  | Zero -> "zero"
  | One -> "one"
  | Two -> "two"
  | Few -> "few"
  | Many -> "many"
  | Other -> "other"

let normalize_lang locale =
  let l = String.lowercase_ascii (String.trim locale) in
  match String.split_on_char '-' l with
  | lang :: _ ->
    (match String.split_on_char '_' lang with hd :: _ -> hd | [] -> lang)
  | [] -> "en"

let plural_category ~locale (n : int) : plural =
  let n = abs n in
  let m10 = n mod 10 and m100 = n mod 100 in
  match normalize_lang locale with
  | "ja" | "zh" | "ko" | "vi" | "th" | "tr" | "id" -> Other
  | "fr" | "hi" -> if n = 0 || n = 1 then One else Other
  | "ru" | "uk" ->
    if m10 = 1 && m100 <> 11 then One
    else if m10 >= 2 && m10 <= 4 && (m100 < 12 || m100 > 14) then Few
    else Many
  | "pl" ->
    if n = 1 then One
    else if m10 >= 2 && m10 <= 4 && (m100 < 12 || m100 > 14) then Few
    else Many
  | "cs" | "sk" ->
    if n = 1 then One else if n >= 2 && n <= 4 then Few else Other
  | "ro" ->
    if n = 1 then One
    else if n = 0 || (m100 >= 1 && m100 <= 19) then Few
    else Other
  | "ar" ->
    if n = 0 then Zero
    else if n = 1 then One
    else if n = 2 then Two
    else if m100 >= 3 && m100 <= 10 then Few
    else if m100 >= 11 && m100 <= 99 then Many
    else Other
  | _ ->
    (* en, de, es, it, pt, nl, sv, ... *)
    if n = 1 then One else Other

(* 3. Message Catalog & Interpolation *)
type catalog = (string * (string * string) list) list

let replace_vars vars text =
  List.fold_left
    (fun acc (k, v) ->
      let needle = "{{" ^ k ^ "}}" in
      let nlen = String.length needle in
      let alen = String.length acc in
      let b = Buffer.create alen in
      let i = ref 0 in
      while !i < alen do
        if !i + nlen <= alen && String.sub acc !i nlen = needle then (
          Buffer.add_string b v;
          i := !i + nlen
        ) else (
          Buffer.add_char b acc.[!i];
          incr i
        )
      done;
      Buffer.contents b)
    text vars

let t ~(catalog : catalog) ?(fallback_locale = "en") ~locale ?count ?(vars = []) key =
  let find_in loc k =
    match List.assoc_opt loc catalog with
    | None -> None
    | Some entries -> List.assoc_opt k entries
  in
  let lookup_key =
    match count with
    | None -> key
    | Some n -> key ^ "." ^ plural_suffix (plural_category ~locale n)
  in
  let all_vars =
    match count with
    | None -> vars
    | Some n -> ("count", string_of_int n) :: vars
  in
  let raw =
    match find_in locale lookup_key with
    | Some s -> s
    | None ->
      (match find_in locale (key ^ ".other") with
       | Some s -> s
       | None ->
         (match find_in fallback_locale lookup_key with
          | Some s -> s
          | None ->
            (match find_in fallback_locale key with
             | Some s -> s
             | None -> key)))
  in
  replace_vars all_vars raw

(* 4. Pure-OCaml UTC Calendar & RFC 7231 / ISO-8601 Date Formatting
   Howard Hinnant's civil-from-days algorithm in Int64. *)
type utc_datetime = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  weekday : int; (* 0 = Sun .. 6 = Sat *)
}

let utc_of_epoch (sec64 : int64) : utc_datetime =
  let floor_div a b =
    let q = Int64.div a b and r = Int64.rem a b in
    if r < 0L then Int64.sub q 1L else q
  in
  let days = floor_div sec64 86400L in
  let sod = Int64.to_int (Int64.sub sec64 (Int64.mul days 86400L)) in
  let hour = sod / 3600 in
  let minute = (sod mod 3600) / 60 in
  let second = sod mod 60 in
  let weekday =
    let w = Int64.to_int (Int64.rem (Int64.add days 4L) 7L) in
    if w < 0 then w + 7 else w
  in
  let z = Int64.add days 719468L in
  let era = floor_div z 146097L in
  let doe = Int64.to_int (Int64.sub z (Int64.mul era 146097L)) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (Int64.to_int era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = mp + (if mp < 10 then 3 else -9) in
  let year = y + (if m <= 2 then 1 else 0) in
  { year; month = m; day = d; hour; minute; second; weekday }

let to_iso8601 (sec64 : int64) : string =
  let dt = utc_of_epoch sec64 in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    dt.year dt.month dt.day dt.hour dt.minute dt.second

let weekday_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let month_names = [| ""; "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let to_http_date (sec64 : int64) : string =
  let dt = utc_of_epoch sec64 in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday_names.(dt.weekday) dt.day month_names.(dt.month)
    dt.year dt.hour dt.minute dt.second

(* Pure-OCaml ISO-8601 UTC timestamp parser (`YYYY-MM-DD` or `YYYY-MM-DDTHH:MM:SSZ`) *)
let parse_iso8601 (s : string) : int64 option =
  let s = String.trim s in
  let days_from_civil y m d =
    let y = y - (if m <= 2 then 1 else 0) in
    let era = (if y >= 0 then y else y - 399) / 400 in
    let yoe = y - (era * 400) in
    let doy = ((153 * (m + (if m > 2 then -3 else 9)) + 2) / 5) + d - 1 in
    let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
    Int64.sub (Int64.add (Int64.mul (Int64.of_int era) 146097L) (Int64.of_int doe)) 719468L
  in
  let parse_ymd ymd =
    match String.split_on_char '-' ymd with
    | [ ys; ms; ds ] when String.length ys = 4 && String.length ms = 2 && String.length ds = 2 ->
      (match (int_of_string_opt ys, int_of_string_opt ms, int_of_string_opt ds) with
       | Some y, Some m, Some d when m >= 1 && m <= 12 && d >= 1 && d <= 31 ->
         Some (y, m, d)
       | _ -> None)
    | _ -> None
  in
  let parse_hms hms =
    let hms =
      let n = String.length hms in
      if n > 0 && (hms.[n - 1] = 'Z' || hms.[n - 1] = 'z') then String.sub hms 0 (n - 1) else hms
    in
    let hms = match String.split_on_char '.' hms with hd :: _ -> hd | [] -> hms in
    match String.split_on_char ':' hms with
    | [ hs; ms; ss ] ->
      (match (int_of_string_opt hs, int_of_string_opt ms, int_of_string_opt ss) with
       | Some h, Some m, Some sec when h >= 0 && h <= 23 && m >= 0 && m <= 59 && sec >= 0 && sec <= 60 ->
         Some (h, m, sec)
       | _ -> None)
    | _ -> None
  in
  match String.split_on_char 'T' s with
  | [ ymd ] ->
    (match parse_ymd ymd with
     | Some (y, m, d) -> Some (Int64.mul (days_from_civil y m d) 86400L)
     | None -> None)
  | [ ymd; hms ] ->
    (match (parse_ymd ymd, parse_hms hms) with
     | Some (y, m, d), Some (hh, mm, ss) ->
       let days = days_from_civil y m d in
       Some (Int64.add (Int64.mul days 86400L) (Int64.of_int ((hh * 3600) + (mm * 60) + ss)))
     | _ -> None)
  | _ -> None

(* Locale-aware number & decimal grouping across major locales *)
let format_number ~locale ?(decimals = 0) (v : float) : string =
  let neg = v < 0.0 in
  let av = abs_float v in
  let factor = 10.0 ** float_of_int (max 0 decimals) in
  let rounded = floor ((av *. factor) +. 0.5) /. factor in
  let raw = Printf.sprintf "%.*f" (max 0 decimals) rounded in
  let int_part, frac_part =
    match String.split_on_char '.' raw with
    | [ ip; fp ] -> (ip, Some fp)
    | _ -> (raw, None)
  in
  let sep_group, sep_dec =
    let base =
      match String.split_on_char '-' (String.lowercase_ascii locale) with
      | b :: _ -> b
      | [] -> "en"
    in
    match base with
    | "de" | "es" | "pt" | "it" | "nl" | "tr" | "vi" | "id" -> (".", ",")
    | "fr" | "ru" | "pl" | "uk" -> (" ", ",")
    | _ -> (",", ".")
  in
  let ilen = String.length int_part in
  let buf = Buffer.create (ilen + 8) in
  for i = 0 to ilen - 1 do
    if i > 0 && (ilen - i) mod 3 = 0 then Buffer.add_string buf sep_group;
    Buffer.add_char buf int_part.[i]
  done;
  let core =
    match frac_part with
    | Some fp when decimals > 0 -> Buffer.contents buf ^ sep_dec ^ fp
    | _ -> Buffer.contents buf
  in
  if neg then "-" ^ core else core
