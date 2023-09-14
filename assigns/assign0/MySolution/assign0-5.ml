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
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)

(* ****** ****** *)

let stringrev (cs: string): string =
  let len = string_length cs in
  let rec revhelper i revresult =
    if i < 0 then
      revresult
    else
      let c = string_get (cs, i) in
      revhelper (i - 1) (revresult ^ str c)
  in
  revhelper (len - 1) ""
;;

(* end of [CS320-2023-Fall-assign0.ml] *)
