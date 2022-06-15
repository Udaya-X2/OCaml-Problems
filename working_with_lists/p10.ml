(* Run-length encoding *)

(* If you need so, refresh your memory about run-length encoding:
   http://en.wikipedia.org/wiki/Run-length_encoding *)

(* Here is an example: *)

(* # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];; *)
(* - : (int * string) list = *)
(* [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

(* My solution *)
let encode (l : 'a list) : (int * 'a) list =
  let rec aux (n : int) (data : 'a) = function
    | hd :: tl ->
        if hd = data then aux (n + 1) hd tl else (n, data) :: aux 1 hd tl
    | [] -> [(n, data)]
  in
  match l with
  | hd :: tl -> aux 1 hd tl
  | [] -> []

(* Website solutions *)
let encode list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((count + 1, a) :: acc) t in
  List.rev (aux 0 [] list);;

(* An alternative solution, which is shorter but requires more memory,
   is to use the pack function declared in problem 9: *)
let encode list =
  List.map (fun l -> (List.length l, List.hd l)) (pack list)
