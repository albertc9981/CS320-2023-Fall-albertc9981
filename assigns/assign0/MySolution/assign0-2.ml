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
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)

let isPrime (n0: int): bool =
  let rec helper n divisor =
    if divisor * divisor > n0 then true
    else if n0 mod divisor = 0 then false
    else helper n (divisor + 1)
  in
  if n0 <= 1 then false
  else if n0 <= 3 then true
  else if n0 mod 2 = 0 || n0 mod 3 = 0 then false
  else helper n0 5

(* ****** ****** *)
