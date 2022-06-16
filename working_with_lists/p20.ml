(* Remove the K'th element from a list *)

(* Remove the K'th element from a list. *)

(* The first element of the list is numbered 0, the second 1,... *)

(* # remove_at 1 ["a"; "b"; "c"; "d"];; *)
(* - : string list = ["a"; "c"; "d"] *)

(* My solution *)
let rec remove_at (k : int) (l : 'a list) : 'a list =
  match l with
  | hd :: tl -> if k = 0 then tl else hd :: remove_at (k - 1) tl
  | [] -> []

(* Website solution *)
let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t
