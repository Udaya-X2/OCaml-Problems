(* Drop every N'th element from a list *)

(* Drop every N'th element from a list. *)

(* # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)

(* My solution *)
let drop (l : 'a list) (n : int) : 'a list =
  let aux (k : int) = function
    | hd :: tl -> if k = n then aux 
    | [] ->

(* Website solution *)
let drop list n =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = n then aux 1 t else h :: aux (i + 1) t  in
  aux 1 list
