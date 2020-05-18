open Fbast;;

exception NotClosed;;
exception Bug;;

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
    | _ -> true
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
            | _ -> raise Bug)
        | Minus(e1, e2) ->( 
            match eval e1, eval e2 with
            | Int(a), Int(b) -> Int(a - b)
            | _ -> raise Bug)
        | Equal(e1, e2) ->( 
            match eval e1, eval e2 with
            | Int(a), Int(b) -> Bool(a = b)
            | _ -> raise Bug)
        | Not(a) ->(
            match eval a with
            | Bool true -> Bool false
            | Bool false -> Bool true
            | _ -> raise Bug)
        | And(e1,e2) ->(
                match eval e1, eval e2 with
                | Bool true, Bool true -> Bool true
                | Bool true, Bool false -> Bool false
                | Bool false, Bool true -> Bool false
                | Bool false, Bool false -> Bool false
                | _ -> raise Bug)
        | Or(e1,e2) ->(
                match eval e1, eval e2 with
                | Bool true, Bool true -> Bool true
                | Bool true, Bool false -> Bool true
                | Bool false, Bool true -> Bool true
                | Bool false, Bool false -> Bool false
                | _ -> raise Bug)
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
        | LetRec(a, b, e1, e2) -> subst b e1 (eval e2)
        | Int(a) -> Int(a)
        | Bool(a) -> Bool(a)
        | _ -> raise Bug
    else
        raise NotClosed
;;