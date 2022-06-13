(* Palindrome *)

(* Find out whether a list is a palindrome. *)

(* HINT: a palindrome is its own reverse. *)

(* My solution *)
let is_palindrome (l : 'a list) : bool =
  l = List.rev l

(* Website solution *)
let is_palindrome list =
  (* One can use either the rev function from the previous problem, or the built-in List.rev *)
  list = List.rev list;;
