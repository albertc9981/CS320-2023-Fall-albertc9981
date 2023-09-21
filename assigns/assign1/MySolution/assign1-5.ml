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

let string_longest_ascend xs =
  let len = string_length xs in
  if len = 0 then
    ""
  else
    let rec find_longest_ascend start_idx current_idx longest current =
      if current_idx = len then
        if string_length current > string_length longest then
          current
        else
          longest
      else
        let current_char = xs.[current_idx] in
        let prev_char = xs.[current_idx - 1] in
        if current_char >= prev_char then
          find_longest_ascend start_idx (current_idx + 1) longest (current ^ String.make 1 current_char)
        else
          let new_longest =
            if string_length current > string_length longest then
              current
            else
              longest
          in
          find_longest_ascend current_idx (current_idx + 1) new_longest (String.make 1 current_char)
    in
    find_longest_ascend 0 1 (String.make 1 xs.[0]) (String.make 1 xs.[0])
(* ****** ****** *)
