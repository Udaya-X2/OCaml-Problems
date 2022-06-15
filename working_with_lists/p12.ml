(* Decode a run-length encoded list *)

(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)

(* #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)
(* - : string list = *)
(* ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

(* My solution *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode (l : 'a rle list) : 'a list =
  let rec expand = function
    | One o -> [o]
    | Many (n, data) ->
        if n > 1 then data :: expand (Many (n - 1, data))
        else [data]
  in
  List.flatten (List.map expand l) 

(* Website solution *)
let decode list =
  let rec many acc n x =
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
    aux [] (List.rev list);;
