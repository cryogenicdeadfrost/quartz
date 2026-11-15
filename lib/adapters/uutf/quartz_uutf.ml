(* Unicode UTF-8 / Uucp Adapter (`quartz.intl_uutf`) implementing `Quartz_core.Io.UNICODE`.
   Uses `uutf` for lossless UTF-8 streaming decode/encode and `uucp` for Unicode
   case folding and alphabetic classification. *)

open Quartz_core

module Uutf_unicode : Io.UNICODE = struct
  let is_valid_utf8 (s : string) : bool =
    Uutf.String.fold_utf_8
      (fun ok _ chunk ->
        match chunk with
        | `Uchar _ -> ok
        | `Malformed _ -> false)
      true s

  let char_count (s : string) : int =
    Uutf.String.fold_utf_8 (fun acc _ _ -> acc + 1) 0 s

  let casefold (s : string) : string =
    let buf = Buffer.create (String.length s) in
    Uutf.String.fold_utf_8
      (fun () _ chunk ->
        match chunk with
        | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep
        | `Uchar u ->
          (match Uucp.Case.Fold.fold u with
           | `Self -> Uutf.Buffer.add_utf_8 buf u
           | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 buf) us))
      () s;
    Buffer.contents buf

  let slugify (s : string) : string =
    let folded = casefold s in
    let buf = Buffer.create (String.length folded) in
    let prev_dash = ref true in
    Uutf.String.fold_utf_8
      (fun () _ chunk ->
        match chunk with
        | `Malformed _ -> ()
        | `Uchar u ->
          let cp = Uchar.to_int u in
          if cp < 128 && ((cp >= 97 && cp <= 122) || (cp >= 48 && cp <= 57)) then (
            Buffer.add_char buf (Char.chr cp);
            prev_dash := false
          ) else if Uucp.Alpha.is_alphabetic u then (
            Uutf.Buffer.add_utf_8 buf u;
            prev_dash := false
          ) else if not !prev_dash then (
            Buffer.add_char buf '-';
            prev_dash := true
          ))
      () folded;
    let out = Buffer.contents buf in
    let n = String.length out in
    if n > 0 && out.[n - 1] = '-' then String.sub out 0 (n - 1) else out
end
