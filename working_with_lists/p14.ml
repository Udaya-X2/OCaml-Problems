(* Duplicate the elements of a list *)

(* Duplicate the elements of a list. *)

(* # duplicate ["a"; "b"; "c"; "c"; "d"];; *)
(* - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

(* My solution *)
let rec duplicate (l : 'a list) : 'a list =
  match l with
  | hd :: tl -> hd :: (hd :: duplicate tl)
  | [] -> []

(* Website solution *)
let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t
