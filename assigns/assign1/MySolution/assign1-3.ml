#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

let rec is_132 s i j k =
  if i >= string_length s || j >= string_length s || k >= string_length s then
    false
  else
    let a = ord s.[i] in
    let b = ord s.[j] in
    let c = ord s.[k] in
    if a < c && c < b then
      true
    else
      is_132 s i (j + 1) k || is_132 s i j (k + 1) || is_132 s (i + 1) j k

let string_avoid_132 cs = 
      let len = string_length cs in
      not (is_132 cs 0 1 2 || is_132 cs 0 2 1 || is_132 cs 1 0 2 ||
           is_132 cs 1 2 0 || is_132 cs 2 0 1 || is_132 cs 2 1 0 || len < 3)

(* ****** ****** *)