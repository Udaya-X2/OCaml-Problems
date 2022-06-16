(* Split a list into two parts; the length of the first part is given *)

(* Split a list into two parts; the length of the first part is given. *)

(* If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)

(* # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)
(* - : string list * string list = *)
(* (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
(* # split ["a"; "b"; "c"; "d"] 5;; *)
(* - : string list * string list = (["a"; "b"; "c"; "d"], []) *)

(* My solution *)
let split (l : 'a list) (len : int) : 'a list * 'a list =
  let rec aux part_1 part_2 curr_len =
    match part_2 with
    | hd :: tl ->
        if curr_len < len then
          aux (hd :: part_1) tl (curr_len + 1) 
        else
          (List.rev part_1, part_2)
    | [] -> (List.rev part_1, part_2)
  in
  aux [] l 0

(* Website solution *)
let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i - 1) (h :: acc) t 
  in
    aux n [] list
