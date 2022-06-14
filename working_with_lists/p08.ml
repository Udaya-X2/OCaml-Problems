(* Eliminate duplicates *)

(* Eliminate consecutive duplicates of list elements. *)

(* # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];; *)
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

(* My solution *)
let compress (l : 'a list) : 'a list =
  let rec aux (prev : 'a) = function
    | [] -> []
    | hd :: tl -> if hd = prev then aux hd tl else hd :: aux hd tl in
  match l with
  | [] -> []
  | hd :: tl -> hd :: aux hd tl

(* Website solution *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller
