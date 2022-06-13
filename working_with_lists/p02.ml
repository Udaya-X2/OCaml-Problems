(* Last two elements of a list *)

(* Find the last but one (last and penultimate) elements of a list. *)

(* # last_two ["a"; "b"; "c"; "d"];; *)
(* - : (string * string) option = Some ("c", "d") *)
(* # last_two ["a"];; *)
(* - : (string * string) option = None *)

(* My solution *)
let rec last_two (l : 'a list) : ('a * 'a) option =
  match l with
  | [a; b] -> Some (a, b)
  | hd :: tl -> last_two tl
  | _ -> None

(* Website solution *)
let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | _ :: t -> last_two t;;
