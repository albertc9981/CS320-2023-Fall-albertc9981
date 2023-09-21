#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

let string_merge cs1 cs2 =
  let len1 = string_length cs1 in
  let len2 = string_length cs2 in

  let result = string_make_fwork (fun append_char ->
    let rec merge i1 i2 =
      if i1 < len1 && i2 < len2 then
        if cs1.[i1] <= cs2.[i2] then (
          append_char cs1.[i1];
          merge (i1 + 1) i2
        ) else (
          append_char cs2.[i2];
          merge i1 (i2 + 1)
        )
      else if i1 < len1 then(
        append_char cs1.[i1];
        merge (i1 + 1) i2
      )
      else if i2 < len2 then (
        append_char cs2.[i2];
        merge i1 (i2 + 1)
      )
    in
    merge 0 0
    ) in
  result

(* ****** ****** *)