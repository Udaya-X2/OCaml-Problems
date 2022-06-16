(* Replicate the elements of a list a given number of times *)

(* Replicate the elements of a list a given number of times. *)

(* # replicate ["a"; "b"; "c"] 3;; *)
(* - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)

(* My solution *)
let replicate (l : 'a list) (n : int) : 'a list =
  let rec dup (data : 'a) (k : int) (acc : 'a list) : 'a list =
    if k > 0 then dup data (k - 1) (data :: acc) else acc
  in
  let rec iter (acc : 'a list) = function
  | hd :: tl -> iter (dup hd n acc) tl
  | [] -> acc
  in
  List.rev (iter [] l)

(* Website solution *)
let replicate list n =
  let rec prepend n acc x =
    if n = 0 then acc else prepend (n-1) (x :: acc) x in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (prepend n acc h) t in
  (* This could also be written as:
     List.fold_left (prepend n) [] (List.rev list) *)
  aux [] (List.rev list)
