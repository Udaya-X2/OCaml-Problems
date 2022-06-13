(* Reverse a list *)

(* Reverse a list. *)

(* OCaml standard library has List.rev but we ask that you reimplement it. *)

(* # rev ["a"; "b"; "c"];; *)
(* - : string list = ["c"; "b"; "a"] *)

(* My solution *)
let rev (l : 'a list) : 'a list =
  let rec rev_iter (l1 : 'a list) (l2 : 'a list) : 'a list =
    match l1 with
    | [] -> l2
    | hd :: tl -> rev_iter tl (hd :: l2) in
  rev_iter l []

(* Website solution *)
let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list
