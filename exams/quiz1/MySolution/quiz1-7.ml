#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

   (* fixed up some syntax, however, image for this page was very blurry 
   so I was unable to compare with my paper. I am still pretty sure the 
   overall code flow/logic is the same*)
let isPrime(n) =
  let test(i:int): bool = 
    let rec helper n divisor =
      if divisor * divisor > i then true
      else if i mod divisor = 0 then false
      else helper n (divisor + 1)
    in
    if i <= 1 then false
    else if i <= 3 then true
    else if i mod 2 = 0 || i mod 3 = 0 then false
    else helper i 5
  in
  if n < 2 then false else int1_forall(n)(test)

(* ************************************************ *)
