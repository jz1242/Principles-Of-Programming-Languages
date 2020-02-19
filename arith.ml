type expr =
  | Integer of int
  | Negate of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
;;

let rec uses_times (e : expr) : bool = 
    match e with 
    | Integer(a) -> false
    | Times(a, b) -> true 
    | x -> 
        match x with
        | Plus(a, b) -> uses_times(a) || uses_times(b)
        | Negate(a) -> uses_times(a)
        | Minus(a, b) -> uses_times(a) || uses_times(b)

;;

let rec unique_list l = 
    match l with
    | [] -> []
    | a :: b -> a :: unique_list (List.filter(function x -> x != a) l) 

;;
let rec get_all_integers (e : expr) : int list = 
    match e with 
    | Integer(a) -> [a]
    | x -> 
        match x with
        | Plus(a, b) -> unique_list (get_all_integers(a) @ get_all_integers(b))
        | Negate(a) -> unique_list (get_all_integers(a))
        | Minus(a, b) -> unique_list (get_all_integers(a) @ get_all_integers(b))
        | Times(a, b) -> unique_list (get_all_integers(a) @ get_all_integers(b))
;;

let rec pretty_print (e : expr) : string = 
    match e with 
    | Integer(a) -> string_of_int a
    | Plus(a, b) -> "(" ^ pretty_print a ^ " + " ^ pretty_print b ^ ")"
    | Negate(a) -> "-" ^ pretty_print a
    | Minus(a, b) -> "(" ^ pretty_print a ^ " - " ^ pretty_print b ^ ")"
    | Times(a, b) -> "(" ^ pretty_print a ^ " * " ^ pretty_print b ^ ")"
;;

let rec eval (e : expr) : int =     
    match e with 
    | Integer(a) -> a
    | Plus(a, b) -> eval a + eval b
    | Negate(a) -> -1 * eval a 
    | Minus(a, b) -> eval a - eval b
    | Times(a, b) -> eval a * eval b
;;
