(* Run-length encoding of a list (direct solution) *)

(* Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X. *)

(* # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; *)
(* - : string rle list = *)
(* [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] *)

(* My solution *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (l : 'a list) : 'a rle list =
  let mk_node (n : int) (data : 'a) : 'a rle =
    match n with
    | 1 -> One data
    | _ -> Many (n, data) in
  let rec aux (n : int) (data : 'a) = function
    | hd :: tl ->
        if hd = data then
          aux (n + 1) hd tl 
        else
          mk_node n data :: aux 1 hd tl
    | [] -> [mk_node n data]
  in
  match l with
  | [] -> []
  | hd :: tl -> aux 1 hd tl

(* Website solution *)
let encode list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 (rle count a :: acc) t
  in
    List.rev (aux 0 [] list)
