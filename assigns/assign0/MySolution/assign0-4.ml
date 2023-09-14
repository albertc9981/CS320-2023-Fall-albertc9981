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
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
for natural numbers x.
(You can assume that the given string is a sequence of digits)
(And the empty sequence represents the integer 0)
*)

let str2int(cs: string): int =
  let len = string_length cs in
  let rec myloop i intresult = 
    if i >= len then intresult
    else
      let digit = ord (string_get (cs, i)) - ord '0' in
      myloop (i + 1) (intresult * 10 + digit)
  in
  if len = 0 then 0
  else myloop 0 0
;;
(* ****** ****** *)