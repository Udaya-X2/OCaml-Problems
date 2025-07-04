(* Extract a slice from a list *)

(* Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original
   list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements). *)

(* # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;; *)
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)

(* My solution *)
let slice (l : 'a list) (i : int) (k : int) : 'a list =
  let rec aux (count : int) = function
    | hd :: tl -> 
        if count > k then []
        else if count >= i then hd :: aux (count + 1) tl
        else aux (count + 1) tl
    | [] -> []
  in
  aux 0 l

(* Website solution *)
let slice list i k =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n - 1) t
  in
  take (k - i + 1) (drop i list)

(* This solution has a drawback, namely that the take function is not tail recursive so it may exhaust the stack when given a very long list. You may also
   notice that the structure of take and drop is similar and you may want to abstract their common skeleton in a single function. Here is a solution. *)

let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l -> if n = 0 then (acc, l)
                   else fold_until f (f acc h) (n - 1) t
                   let slice list i k =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken
