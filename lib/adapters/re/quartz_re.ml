(* Pure-OCaml PCRE Regex Adapter (`quartz.re`) implementing `Quartz_core.Io.REGEX`
   and providing PCRE-backed Quartz form validators. *)

open Quartz_core

module Re_regex : Io.REGEX = struct
  let matches ~pattern input =
    let rx = Re.Pcre.regexp pattern in
    Re.execp rx input

  let find_all ~pattern input =
    let rx = Re.Pcre.regexp pattern in
    Re.matches rx input

  let replace_all ~pattern ~with_str input =
    let rx = Re.Pcre.regexp pattern in
    Re.replace_string rx ~by:with_str input
end

let pcre_validator ~pattern ?(msg = "Does not match required pattern") : string Form.validator =
  let rx = Re.Pcre.regexp pattern in
  fun s -> if Re.execp rx s then Ok s else Error msg
