(* N'th lement of a list *)

(* Find the K'th element of a list. *)

(* REMARK: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds. *)

(* # List.nth ["a"; "b"; "c"; "d"; "e"] 2;; *)
(* - : string = "c" *)
(* # List.nth ["a"] 2;; *)
(* Exception: Failure "nth". *)

(* My solution *)
let rec kth (l : 'a list) (k : int) : 'a option =
  match l with
  | [] -> None
  | hd :: tl -> if k == 0 then Some hd else kth tl (k - 1)

(* Website solution *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t;;
