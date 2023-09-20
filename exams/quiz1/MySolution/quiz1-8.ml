(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

(* I fixed up some of the syntax and function names. the overall 
flow and logic of the code is the same. *)
let sort5: int*int*int*int*int -> int*int*int*int*int =
  fun (x1, x2, x3 , x4, x5) ->
    let lst = [x1; x2; x3; x4; x5] in
    let lst_new = List.sort compare lst in
    match lst_new with
      |[x; y; z ;w ;u] -> (x, y, z, w, u)
      | _ -> failwith "Input should countain 5 ints"
  

(* ************************************************ *)
