(* Pack consecutive duplicates *)

(* Pack consecutive duplicates of list elements into sublists. *)

(* # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];; *)
(* - : string list list = *)
(* [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; *)
 (* ["e"; "e"; "e"; "e"]] *)

(* My solution *)
let pack (l : 'a list) : 'a list list =
  let rec aux (elem : 'a list) (sub : 'a list) : 'a list list =
    match elem with
    | a :: (b :: _ as tl) ->
        if a = b then aux tl (a :: sub) else (a :: sub) :: (aux tl [])
    | [x] -> [x :: sub]
    | [] -> []
  in
  aux l []

(* Website solution *)
let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (a :: current) acc t
       else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list);;
