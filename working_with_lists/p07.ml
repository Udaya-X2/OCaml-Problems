(* Flatten a list *)

(* Flatten a nested list structure. *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

(* # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];; *)
(* - : string list = ["a"; "b"; "c"; "d"; "e"] *)

(* My solution *)
let rec flatten (l : 'a node list) : 'a list =
  match l with
  | [] -> []
  | hd :: tl -> match hd with
    | One o -> o :: flatten tl
    | Many m -> flatten m @ flatten tl

(* Website solution *)
(* This function traverses the list, prepending any encountered elements
  to an accumulator, which flattens the list in inverse order. It can
  then be reversed to obtain the actual flattened list. *);;
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list);;
