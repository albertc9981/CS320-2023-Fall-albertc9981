#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)

let intrep_add ds1 ds2 =
  let len1 = string_length ds1 in
  let len2 = string_length ds2 in

  let rec add_digits i j carry result =
    if i < 0 && j < 0 then
      (match carry with 0 -> result | _ -> string_of_int carry ^ result)
    else
      let d1 = if i >= 0 then int_of_char ds1.[i] - int_of_char '0' else 0 in
      let d2 = if j >= 0 then int_of_char ds2.[j] - int_of_char '0' else 0 in
      let sum = d1 + d2 + carry in
      let new_carry, digit = sum / 10, sum mod 10 in
      let new_result =
        if i >= 0 || j >= 0 || digit <> 0 then string_of_int digit ^ result else result
      in
      add_digits (i - 1) (j - 1) new_carry new_result
  in

  let result = add_digits (len1 - 1) (len2 - 1) 0 "" in

  if string_length result = 0 then "0" else result


(* ****** ****** *)