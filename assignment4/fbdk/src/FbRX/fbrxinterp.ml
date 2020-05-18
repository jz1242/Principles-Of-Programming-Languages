(* Assignment 6 Jason Zhang jzhan127
    Collaborators: Dan Qian
 *)
open Fbrxast;;

exception NotClosed

let fbLabelNotFound = Raise("#LabelNotFound", Int(0))
let fbTypeMismatch = Raise("#TypeMismatch", Int(0))

let rec lookupRecord body (Lab l) = 
    match body with
    [] -> failwith "error"
    | (Lab a, v)::t -> if l = a then v else lookupRecord t (Lab l)
;;

let rec checkIfExists a (Lab l) = 
    match a with
    [] -> false
    | (Lab b, v)::t -> if l = b then true else checkIfExists t (Lab l)
;;

let rec appendLists a b = 
    match a with
    | [] -> b
    | (Lab l, v) :: t -> if checkIfExists b (Lab l) = true then appendLists t b  else appendLists t ((Lab l, v) :: b)
;;

let rec check_closed v e =
    match e with
    | _ -> true
    | Plus(e1, e2) -> check_closed v e1 && check_closed v e2
    | Minus(e1, e2) -> check_closed v e1 && check_closed v e2
    | Equal(e1, e2) -> check_closed v e1 && check_closed v e2
    | Appl(e1, e2) -> check_closed v e1 && check_closed v e2
    | Function(a, ex) -> let app = a::v in (check_closed app ex)
    | Let(a, e1, e2) ->( 
        match check_closed v e1 with 
        | true -> let app = a::v in(check_closed app e2)
        | false -> false)
    | If(e1, e2, e3) -> check_closed v e1 && check_closed v e2 && check_closed v e3
    | And(e1, e2) -> check_closed v e1 && check_closed v e2
    | Or(e1, e2) -> check_closed v e1 && check_closed v e2
    | Not(e1) -> check_closed v e1
    | Var(a) -> List.exists(function x -> x = a) v
    | Int(a) -> true
    | Bool(a)-> true
;;

let rec subst v e i =
    match e with
        | Var(a) -> (
            if a = v then
                i
            else
                Var(a)
        )
        | Int(a) -> Int(a)
        | Bool(a) -> Bool(a)
        | Plus(e1, e2) -> Plus(subst v e1 i, subst v e2 i)
        | Minus(e1, e2) -> Minus(subst v e1 i, subst v e2 i)
        | Equal(e1, e2) -> Equal(subst v e1 i, subst v e2 i)
        | Not(a) -> Not(subst v a i)
        | And(e1, e2) -> And(subst v e1 i, subst v e2 i)
        | Or(e1, e2) -> Or(subst v e1 i, subst v e2 i)
        | If(e1, e2, e3) -> If(subst v e1 i, subst v e2 i, subst v e3 i)
        | Let(a, e1, e2) -> 
            if a = v then
                Let(a, subst v e1 i, e2)
            else 
                Let(a, subst v e1 i, subst v e2 i)
        | Appl(e1, e2) -> Appl(subst v e1 i, subst v e2 i)
        | Function(a, ex) -> 
            if a = v then
                Function(a, ex)
            else
                Function(a, subst v ex i)
    ;;

let rec eval e = 
    if check_closed [] e = true then
        match e with
        | Plus(e1, e2) ->( 
            match eval e1, eval e2 with
            | Int(a), Int(b) -> Int(a + b)
            | _ -> failwith "error")
        | Minus(e1, e2) ->( 
            match eval e1, eval e2 with
            | Int(a), Int(b) -> Int(a - b)
            | _ -> failwith "error")
        | Equal(e1, e2) ->( 
            match eval e1, eval e2 with
            | Int(a), Int(b) -> Bool(a = b)
            | _ -> failwith "error")
        | Not(a) ->(
            match eval a with
            | Bool true -> Bool false
            | Bool false -> Bool true
            | _ -> failwith "error")
        | And(e1,e2) ->(
                match eval e1, eval e2 with
                | Bool true, Bool true -> Bool true
                | Bool true, Bool false -> Bool false
                | Bool false, Bool true -> Bool false
                | Bool false, Bool false -> Bool false
                | _ -> failwith "error")
        | Or(e1,e2) ->(
                match eval e1, eval e2 with
                | Bool true, Bool true -> Bool true
                | Bool true, Bool false -> Bool true
                | Bool false, Bool true -> Bool true
                | Bool false, Bool false -> Bool false
                | _ -> failwith "error")
        | Appl(e1, e2) ->(
            match eval e1, eval e2 with
            | Function(a, ex), b -> eval (subst a ex b))
        | Function(a, ex) -> Function (a, ex)
        | Let(a, e1, e2) ->(
            match e1, e2 with
            | inp, ex -> eval (subst a ex inp))
        | If(e1, e2, e3) ->(
            match eval e1, e2, e3 with
            | Bool(a), b, c ->
                if a = true then
                    eval b
                else
                    eval c
            )
        | Int(a) -> Int(a)
        | Bool(a) -> Bool(a)
        | Record(e) -> Record(evalRecord e)
        | Select(l, e) ->(
            match eval e with
            | Record(a) -> lookupRecord a l
            | _ -> failwith "error"

        )
        | Append(a, b) -> (
            match eval a, eval b with
            | Record(c), Record(d) -> Record(appendLists c d)
            | _, _ -> failwith "error"
        )
        | _ -> failwith "error"
    else
        raise NotClosed
and evalRecord body = (
    match body with
    [] -> []
    | (Lab l, e)::t -> (Lab l, eval e)::evalRecord t)
;;