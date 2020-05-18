(* Jason Zhang jzhan127
    Collaborators: Dan Qian

*)
open Tfbsrxast;;
open Tfbsrxpp;;
exception TypeError;;

(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *) 
let rec findType g v = (
    match g with 
    | [] -> raise TypeError
    | (a, b) :: c -> (
        if a = v then
            b
        else
            findType c v
    )

);;

let rec typecheck_g g e =
    match e with
        | Var(a) -> findType g a
        | Bool(a) -> TBool
        | Int(a) -> TInt
        | Not(a) -> typecheck_g g a
        | Or(a, b) -> 
            (match typecheck_g g a, typecheck_g g b with
                | TBool, TBool -> TBool
                | TInt, TInt -> TInt
                | TBottom, TBottom -> (
                    match a, b with
                    | Raise(e, f, g), Raise(h, i, j) -> (
                        if f = i then
                            TBottom
                        else
                            raise TypeError
                    )
                    | _, _ -> raise TypeError
                )
                | TBottom, x -> (
                    match a with 
                    | Raise(e, f, g) -> (
                        if f = x then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | x, TBottom -> (
                    match b with 
                    | Raise(e, f, g) -> (
                        if f = x then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | _, _ -> raise TypeError
            )
        | And(a, b) ->
            (match typecheck_g g a, typecheck_g g b with
                | TBool, TBool -> TBool
                | TInt, TInt -> TInt
                | TBottom, TBottom -> (
                    match a, b with
                    | Raise(e, f, g), Raise(h, i, j) -> (
                        if f = i then
                            TBottom
                        else
                            raise TypeError
                    )
                    | _, _ -> raise TypeError
                )
                | TBottom, x -> (
                    match a with 
                    | Raise(e, f, g) -> (
                        if f = x then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | x, TBottom -> (
                    match b with 
                    | Raise(e, f, g) -> (
                        if f = x then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | _, _ -> raise TypeError
            )
        | Plus(a, b) ->
            (match typecheck_g g a, typecheck_g g b with
                | TInt, TInt -> TInt
                | TBottom, TBottom -> (
                    match a, b with
                    | Raise(e, f, g), Raise(h, i, j) -> (
                        if f = i && f = TInt then
                            TBottom
                        else
                            raise TypeError
                    )
                    | _, _ -> raise TypeError
                )
                | TBottom, x -> (
                    match a with 
                    | Raise(e, f, g) -> (
                        if f = x && x = TInt then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | x, TBottom -> (
                    match b with 
                    | Raise(e, f, g) -> (
                        if f = x && x = TInt then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | _, _ -> raise TypeError
            )
        | Minus(a, b) ->
            (match typecheck_g g a, typecheck_g g b with
            | TInt, TInt -> TInt
            | TBottom, TBottom -> (
                    match a, b with
                    | Raise(e, f, g), Raise(h, i, j) -> (
                        if f = i && f = TInt then
                            TBottom
                        else
                            raise TypeError
                    )
                    | _, _ -> raise TypeError
                )
            | TBottom, x -> (
                    match a with 
                    | Raise(e, f, g) -> (
                        if f = x && x = TInt then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
                | x, TBottom -> (
                    match b with 
                    | Raise(e, f, g) -> (
                        if f = x && x = TInt then
                            x
                        else
                            raise TypeError
                    )
                    | _ -> raise TypeError
                )
            | _, _ -> raise TypeError
        )
        | Equal(a, b) ->
            (match typecheck_g g a, typecheck_g g b with
            | TInt, TInt -> TBool
            | _, _ -> raise TypeError
        )
        | Function (a, b, c) -> 
            (let outputType = typecheck_g ((a, b) :: g) c in TArrow(b, outputType))
        | If(a, b, c) -> 
            (match typecheck_g g a with 
            | TBool -> 
                (let d =  typecheck_g g b in let e = typecheck_g g c in
                if d = e then
                    d
                else
                    (match d, e with
                    | TBottom, TBottom -> (
                        match b, c with
                        | Raise(e, f, g), Raise(h, i, j) -> (
                            if f = i then
                                TBottom
                            else
                                raise TypeError
                        )
                        | _, _ -> raise TypeError
                    )
                    | TBottom, x -> (
                        match b with 
                        | Raise(e, f, g) -> (
                            if f = x then
                                x
                            else
                                raise TypeError
                        )
                        | _ -> raise TypeError
                    )
                    | x, TBottom -> (
                        match c with 
                        | Raise(e, f, g) -> (
                            if f = x then
                                x
                            else
                                raise TypeError
                        )
                        | _ -> raise TypeError
                    )
                    | _, _ -> raise TypeError
                    )
                )
            | _ ->raise TypeError
        )
        | Appl(e1, e2) ->
            (match typecheck_g g e1, typecheck_g g e2 with
            | TArrow(a, b), c -> (
                if a = c then
                    b
                else
                    if c = TBottom then
                        match e2 with
                        | Raise(d, e, f) -> (
                            if a = e then
                                a
                            else
                                raise TypeError
                        )
                        | _ -> raise TypeError
                    else
                        raise TypeError
            )
            | _ -> raise TypeError
        )
        | Raise(a, b, c) -> 
            let outType = typecheck_g g c in (
                if b = outType then
                    TBottom
                else
                    raise TypeError
            )
        | Try(e1, exn, id, typeOf, e2) -> (
            let out1 = (typecheck_g g e1) in let out2 = (typecheck_g ((id, typeOf)::g) e2)
            in (
                match out1, out2 with
                | TBool, TBool -> TBool
                | TInt, TInt -> TInt
                | TBottom, TBottom -> (
                    match e1, e2 with
                    | Raise(e, f, g), Raise(h, i, j) -> (
                        if f = i then
                            TBottom
                        else
                            raise TypeError
                    )
                    | _, _ -> raise TypeError
                )
                | TBottom, x -> x
                | x, TBottom -> x
                | _, _ -> raise TypeError
            )
        )
        | Letrec(id1, id2, fbtype1, e1, fbtype2, e2) ->
            (let outputType1 = typecheck_g ((id1, TArrow(fbtype1, fbtype2))::(id2, fbtype2)::(g)) e1 in let outputType2 =
            typecheck_g ((id1, TArrow(fbtype1, fbtype2))::(id2, fbtype2)::g) e2 in (outputType2))
        | _ -> raise TypeError

;;

let rec typecheck e = 
    (typecheck_g [] e)

;;