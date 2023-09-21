#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)
(*
Assign1: Onward!

Total: 70 points + 20 bonus points
 
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
*)
(* ****** ****** *)

(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)
let intrev10(n: int): int = 
  let rec helper n acc =
    if n = 0 then
        acc
    else
        let digit = n mod 10 in
        let acc_new = acc * 10 + digit in
        let n_new = n / 10 in
        helper n_new acc_new
  in
  helper n 0

(* ****** ****** *)