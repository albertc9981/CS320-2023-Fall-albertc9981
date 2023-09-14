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
fun fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1
*)

(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml returns '0' (due
to arithmetic overflow.
*)

let rec fact(x: int): int =
  if x <= 0 then 1
  else x * fact(x - 1)

let rec find_first_N(n: int): int =
  if fact(n) = 0 then 
    n
  else 
    find_first_N(n + 1)
let myans = find_first_N(0)
  ;;
(* ****** ****** *)
