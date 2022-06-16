(* Rotate a list N places to the left *)

(* Rotate a list N places to the left. *)

(* # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;; *)
(* - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)

(* My solution *)
let rotate (l : 'a list) (n : int) : 'a list =
  let rec aux (k : int) (acc : 'a list) = function
    | hd :: tl -> 
        if k > 0 then aux (k - 1) (hd :: acc) tl
        else hd :: aux k acc tl
    | [] -> List.rev acc
  in 
  match l with
  | [] -> []
  | _ -> aux (n mod List.length l) [] l

(* Website solution *)
let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i - 1) (h :: acc) t  in
  aux n [] list

let rotate list n =
  let len = List.length list in
  (* Compute a rotation value between 0 and len - 1 *)
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a
