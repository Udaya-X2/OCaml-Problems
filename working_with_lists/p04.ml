(* Length of a list *)

(* Find the number of elements of a list. *)

(* OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)

(* # length ["a"; "b"; "c"];; *)
(* - : int = 3 *)
(* # length [];; *)
(* - : int = 0 *)

(* My solution *)
let length (l : 'a list) : int =
  let rec length_iter (n : int) = function
    | [] -> n
    | hd :: tl -> length_iter (n + 1) tl in
  length_iter 0 l

(* Website solution *)
let length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0 list
