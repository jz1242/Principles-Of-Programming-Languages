open Fbrxast;;

let pretty_print_fun f e = (f e "")

let rec pp e pad =
  match e with
  | Var(Ident(x)) -> x
  | Bool(true) -> "True"
  | Bool(false) -> "False"
  | Int(x) -> string_of_int x
  | String(x) -> x

  | Plus(e1, e2) ->
    pp e1 pad ^ " + " ^ pp e2 pad
  | Minus(e1, e2) ->
    pp e1 pad ^ " - " ^ pp e2 pad
  | Equal(e1, e2) ->
    pp e1 pad ^ " = " ^ pp e2 pad

  | And(e1, e2) ->
    pp e1 pad ^ " And " ^ pp e2 pad
  | Or(e1, e2) ->
    pp e1 pad ^ " Or " ^ pp e2 pad
  | Not(e1) ->   "Not " ^ pp e1 pad

  | Appl(e1, e2) ->
    "(" ^ pp e1 pad ^ ") (" ^ pp e2 pad ^ ")"
  | Function(Ident(i), x) ->
    let newpad = pad ^ "  " in
    "Function " ^ i ^ " ->\n" ^ newpad ^ pp x newpad
  | If(e1, e2, e3) ->
    let newpad = pad ^ "  " in
    "If " ^ pp e1 pad ^ " Then\n" ^ newpad ^ pp e2 newpad ^
    "\n" ^ pad ^ "Else\n" ^ newpad ^ pp e3 newpad
  | Let(Ident i, e1, e2) ->
    let newpad = pad ^ "  " in 
    ("Let " ^ i ^ " = " ^ pp e1 pad ^ " In\n" ^ newpad ^ pp e2 newpad) 
  | LetRec(Ident(i1), Ident(i2), e1, e2) -> 
    let newpad = pad ^ "  " in
    "Let Rec " ^ i1 ^ " " ^ i2 ^ " =\n" ^ newpad ^ pp e1 newpad ^ "\n" ^
    pad ^ "In\n" ^ newpad ^ pp e2 newpad

  | Record body -> "{" ^ pp_record body pad^ "}"
  | Select(Lab l, e) -> pp e pad ^ "." ^ l
  | Append (e1, e2) -> pp e1 pad ^ " @ " ^ pp e2 pad

  | Raise(n, e) -> "Raise " ^ n ^ " " ^ (pp e pad)
  | Try(e1, n, Ident id, e2) ->
    "Try\n" ^ (pp e1 pad) ^ "\n" ^ pad ^  "With " ^  id ^ " -> " ^ (pp e2 pad)

and pp_record body pad =
  match body with
    [] -> ""
  | (Lab l, e)::rest ->
    l ^ "=" ^ pp e pad ^
    (if rest = [] then "" else "; ") ^
    pp_record rest pad



let pretty_print e = pretty_print_fun pp e

let rec pp_type t pad =
  match t with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow(t1, t2) ->
    (
      match t1 with
      | TArrow(_,_) -> "(" ^ pp_type t1 pad ^ ")"
      | _ -> pp_type t1 pad
    ) ^ " -> " ^ pp_type t2 pad
  | TVar(s) -> "'" ^ s

let pretty_print_type t = pretty_print_fun pp_type t
