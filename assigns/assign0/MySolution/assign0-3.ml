(* ****** ****** *)
(*
Assign0: Warmup!
*)
(* ****** ****** *)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

let int2str(i0: int): string =
  let rec helper n strresult =
    if n = 0 then
      strresult
    else
      let digit = n mod 10 in
      let char_digit = chr (ord '0' + digit) in
    helper (n / 10) (str char_digit ^ strresult)
  in
  if i0 = 0 then
    "0"
  else if i0 < 0 then
    "-" ^ helper (-i0) ""
else
  helper i0 ""
;;

(* ****** ****** *)