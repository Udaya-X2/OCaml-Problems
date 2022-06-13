(* Tail of a list *)

(* Write a function last : 'a list -> 'a option that returns the last element of a list *)

(* # last ["a" ; "b" ; "c" ; "d"];; *)
(* - : string option = Some "d" *)
(* # last [];; *)
(* - : 'a option = None *)

(* My solution *)
let rec last (l : 'a list) : 'a option =
  match l with
  | hd :: [] -> Some hd
  | hd :: tl -> last tl
  | [] -> None

(* Website solution *)
let rec last = function 
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
