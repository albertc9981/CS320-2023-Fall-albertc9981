#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

let rec helper xs start_i curr_i long_i =
  let xs_len = string_length xs in
  if curr_i = xs_len then
    let longest_sub =
      string_init (longest_i - start_i + 1) (fun i -> xs.[start_i + i])
    in
    longest_sub
  else
    let curr_char = xs.[curr_i] in
    let prev_char = xs.[curr_i - 1] in
    if ord curr_char >= ord prev_char then
      let new_longest_i =
        if curr_i - start_i + 1 > longest_i - start_i + 1 then curr_i else longest_i
      in
      helper xs start_i (curr_i + 1) new_longest_i
    else
      let new_start_i = curr_i in
      helper xs new_start_i (curr_i + 1) longest_i

let string_longest_ascend xs =
  match xs with
  | "" -> ""
  | _ ->
      let start_i = 0 in
      let curr_i = 1 in
      let longest_i = 0 in
      helper xs start_i curr_i longest_i
(* ****** ****** *)
