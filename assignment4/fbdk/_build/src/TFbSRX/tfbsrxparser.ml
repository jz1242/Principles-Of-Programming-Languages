
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | TRY
    | THEN
    | SET
    | SEMI
    | RPAREN
    | REF
    | REC
    | RCURLY
    | RAISE
    | PLUS
    | OR
    | NOT
    | MINUS
    | LPAREN
    | LET
    | LCURLY
    | INTTYPE
    | INT of (
# 43 "src/TFbSRX/tfbsrxparser.mly"
       (int)
# 29 "src/TFbSRX/tfbsrxparser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 36 "src/TFbSRX/tfbsrxparser.ml"
  )
    | GOESTO
    | GET
    | FUNCTION
    | EXN of (
# 62 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 44 "src/TFbSRX/tfbsrxparser.ml"
  )
    | EQUAL
    | EOEX
    | ELSE
    | DOT
    | COLON
    | BOOLTYPE
    | BOOL of (
# 32 "src/TFbSRX/tfbsrxparser.mly"
       (bool)
# 55 "src/TFbSRX/tfbsrxparser.ml"
  )
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState98
  | MenhirState95
  | MenhirState89
  | MenhirState87
  | MenhirState84
  | MenhirState81
  | MenhirState79
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState56
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState37
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState19
  | MenhirState16
  | MenhirState12
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "src/TFbSRX/tfbsrxparser.mly"
  

open Tfbsrxast;;

let rec type_string t =
	match t with
	| TInt -> "Int"
	| TBool -> "Bool"
	| TArrow(t1, t2) ->
			(
				match t1 with
					| TArrow(_,_) -> "(" ^ type_string t1 ^ ")"
					| _ -> type_string t1
			) ^ " -> " ^ (type_string t2)
	| TRef(t) -> (type_string t) ^ " Ref"
  | TRec(lst) ->
		"{" ^ (List.fold_left (fun acc -> fun (Lab(lbl),typ) ->
				acc ^ (if String.length acc > 0 then ", " else "") ^
				lbl ^ ":" ^ (type_string typ)
			) "" lst) ^ "}"
  | TBottom -> "BOTTOM"

let mkexn n t =
  "#" ^ n ^ "@" ^ (type_string t)

# 143 "src/TFbSRX/tfbsrxparser.ml"

let rec _menhir_goto_record_body : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Tfbsrxast.label * Tfbsrxast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : ((Tfbsrxast.label * Tfbsrxast.expr) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 141 "src/TFbSRX/tfbsrxparser.mly"
      ( Record _2 )
# 164 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.label))), _, (_3 : (Tfbsrxast.expr))), _, (_5 : ((Tfbsrxast.label * Tfbsrxast.expr) list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Tfbsrxast.label * Tfbsrxast.expr) list) = 
# 154 "src/TFbSRX/tfbsrxparser.mly"
      ( (_1, _3)::_5 )
# 182 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | BOOLTYPE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | INTTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LCURLY ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | RCURLY ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_goto_exn_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Tfbsrxast.fbtype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (string * Tfbsrxast.fbtype))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string * Tfbsrxast.fbtype) = 
# 198 "src/TFbSRX/tfbsrxparser.mly"
       ( _2 )
# 479 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_exn_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | FUNCTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | GET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | IF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | LCURLY ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | RAISE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | REF ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | TRY ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GOESTO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | FUNCTION ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | GET ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | IDENT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | IF ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | INT _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | LCURLY ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | LET ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | LPAREN ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | NOT ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | RAISE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | REF ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | TRY ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_record_type : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Tfbsrxast.label * Tfbsrxast.fbtype) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : ((Tfbsrxast.label * Tfbsrxast.fbtype) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.fbtype) = 
# 180 "src/TFbSRX/tfbsrxparser.mly"
      ( TRec _2 )
# 603 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.label))), _, (_3 : (Tfbsrxast.fbtype))), _, (_5 : ((Tfbsrxast.label * Tfbsrxast.fbtype) list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Tfbsrxast.label * Tfbsrxast.fbtype) list) = 
# 191 "src/TFbSRX/tfbsrxparser.mly"
      ( (_1, _3)::_5 )
# 621 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_record_type _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.fbtype) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.fbtype))) = _menhir_stack in
    let _2 = () in
    let _v : (Tfbsrxast.fbtype) = 
# 182 "src/TFbSRX/tfbsrxparser.mly"
      ( TRef _1 )
# 636 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.fbtype) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INTTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LCURLY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 145 "src/TFbSRX/tfbsrxparser.mly"
      ( _2 )
# 687 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Tfbsrxast.expr) = 
# 117 "src/TFbSRX/tfbsrxparser.mly"
      ( Set(_1, _3) )
# 722 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Tfbsrxast.expr) = 
# 97 "src/TFbSRX/tfbsrxparser.mly"
      ( Plus(_1, _3) )
# 739 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Tfbsrxast.expr) = 
# 103 "src/TFbSRX/tfbsrxparser.mly"
      ( Or(_1, _3) )
# 764 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Tfbsrxast.expr) = 
# 99 "src/TFbSRX/tfbsrxparser.mly"
      ( Minus(_1, _3) )
# 781 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | ELSE | EOEX | EQUAL | OR | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Tfbsrxast.expr) = 
# 107 "src/TFbSRX/tfbsrxparser.mly"
      ( Equal(_1, _3) )
# 800 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | OR | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Tfbsrxast.expr) = 
# 101 "src/TFbSRX/tfbsrxparser.mly"
      ( And(_1, _3) )
# 829 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.ident))), _, (_4 : (Tfbsrxast.fbtype))), _, (_6 : (Tfbsrxast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 109 "src/TFbSRX/tfbsrxparser.mly"
      ( Function(_2, _4, _6) )
# 864 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Tfbsrxast.expr) = 
# 119 "src/TFbSRX/tfbsrxparser.mly"
      ( Get _2 )
# 881 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | FUNCTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | GET ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | IF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LCURLY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | NOT ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | REF ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | FUNCTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | GET ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | IF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | LCURLY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NOT ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | REF ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))), _, (_4 : (Tfbsrxast.expr))), _, (_6 : (Tfbsrxast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 113 "src/TFbSRX/tfbsrxparser.mly"
      ( If(_2, _4, _6) )
# 1026 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.label))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : ((Tfbsrxast.label * Tfbsrxast.expr) list) = 
# 152 "src/TFbSRX/tfbsrxparser.mly"
      ( [(_1, _3)] )
# 1070 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INTTYPE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LCURLY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), _, (_3 : (Tfbsrxast.ident))), _, (_4 : (Tfbsrxast.ident))), _, (_6 : (Tfbsrxast.fbtype))), _, (_8 : (Tfbsrxast.expr))), _, (_10 : (Tfbsrxast.fbtype))), _, (_12 : (Tfbsrxast.expr))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 111 "src/TFbSRX/tfbsrxparser.mly"
      ( Letrec(_3, _4, _6, _8, _10, _12) )
# 1148 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Tfbsrxast.expr) = 
# 105 "src/TFbSRX/tfbsrxparser.mly"
      ( Not _2 )
# 1165 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (string * Tfbsrxast.fbtype))), _, (_3 : (Tfbsrxast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 123 "src/TFbSRX/tfbsrxparser.mly"
      ( let (n, t) = _2 in Raise(mkexn n t, t, _3) )
# 1192 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Tfbsrxast.expr) = 
# 115 "src/TFbSRX/tfbsrxparser.mly"
      ( Ref _2 )
# 1209 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXN _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COLON | ELSE | EOEX | RCURLY | RPAREN | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.expr))), _, (_4 : (string * Tfbsrxast.fbtype))), (_5 : (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 1270 "src/TFbSRX/tfbsrxparser.ml"
            ))), _, (_7 : (Tfbsrxast.expr))) = _menhir_stack in
            let _6 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.expr) = 
# 121 "src/TFbSRX/tfbsrxparser.mly"
      ( let (n, t) = _4 in Try(_2, mkexn n t, Ident _5, t, _7) )
# 1278 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | EOEX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 85 "src/TFbSRX/tfbsrxparser.mly"
      (Tfbsrxast.expr)
# 1302 "src/TFbSRX/tfbsrxparser.ml"
            ) = 
# 90 "src/TFbSRX/tfbsrxparser.mly"
            ( _1 )
# 1306 "src/TFbSRX/tfbsrxparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 85 "src/TFbSRX/tfbsrxparser.mly"
      (Tfbsrxast.expr)
# 1313 "src/TFbSRX/tfbsrxparser.ml"
            )) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.label) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.label) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INTTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LCURLY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_goto_type_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Tfbsrxast.fbtype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.label))), _, (_3 : (Tfbsrxast.fbtype))) = _menhir_stack in
            let _2 = () in
            let _v : ((Tfbsrxast.label * Tfbsrxast.fbtype) list) = 
# 189 "src/TFbSRX/tfbsrxparser.mly"
      ( [(_1, _3)] )
# 1420 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_record_type _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | BOOL _ | EQUAL | FUNCTION | GET | IDENT _ | IF | IN | INT _ | LCURLY | LET | LPAREN | NOT | RAISE | RCURLY | RPAREN | SEMI | TRY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.fbtype))), _, (_3 : (Tfbsrxast.fbtype))) = _menhir_stack in
            let _2 = () in
            let _v : (Tfbsrxast.fbtype) = 
# 178 "src/TFbSRX/tfbsrxparser.mly"
      ( TArrow(_1, _3) )
# 1445 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Tfbsrxast.fbtype))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Tfbsrxast.fbtype) = 
# 184 "src/TFbSRX/tfbsrxparser.mly"
      ( _2 )
# 1473 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | BOOL _ | FUNCTION | GET | IDENT _ | IF | INT _ | LCURLY | LET | LPAREN | NOT | RAISE | RPAREN | TRY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 62 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 1496 "src/TFbSRX/tfbsrxparser.ml"
            ))), _, (_2 : (Tfbsrxast.fbtype))) = _menhir_stack in
            let _v : (string * Tfbsrxast.fbtype) = 
# 196 "src/TFbSRX/tfbsrxparser.mly"
       ( (_1, _2) )
# 1501 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_exn_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | FUNCTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | GET ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | IF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | LCURLY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | NOT ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | REF ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | BOOLTYPE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FUNCTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | GET ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | IF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INTTYPE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LCURLY ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LPAREN ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NOT ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | REF ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GOESTO ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | FUNCTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | GET ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | LCURLY ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NOT ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | REF ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | REF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_appl_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | AND | COLON | ELSE | EOEX | EQUAL | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))) = _menhir_stack in
        let _v : (Tfbsrxast.expr) = 
# 95 "src/TFbSRX/tfbsrxparser.mly"
      ( _1 )
# 1688 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 1726 "src/TFbSRX/tfbsrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INTTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LCURLY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Tfbsrxast.expr) = 
# 143 "src/TFbSRX/tfbsrxparser.mly"
      ( Record [] )
# 1756 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 1763 "src/TFbSRX/tfbsrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 1771 "src/TFbSRX/tfbsrxparser.ml"
    )) = _v in
    let _v : (Tfbsrxast.label) = 
# 159 "src/TFbSRX/tfbsrxparser.mly"
      ( Lab _1 )
# 1776 "src/TFbSRX/tfbsrxparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_3 : (Tfbsrxast.label))) = _menhir_stack in
        let _2 = () in
        let _v : (Tfbsrxast.expr) = 
# 147 "src/TFbSRX/tfbsrxparser.mly"
      ( Select(_3, _1) )
# 1829 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INTTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LCURLY ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Tfbsrxast.fbtype) = 
# 174 "src/TFbSRX/tfbsrxparser.mly"
      ( TInt )
# 1880 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Tfbsrxast.fbtype) = 
# 176 "src/TFbSRX/tfbsrxparser.mly"
      ( TBool )
# 1892 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_type_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Tfbsrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState98 | MenhirState1 | MenhirState2 | MenhirState26 | MenhirState27 | MenhirState28 | MenhirState89 | MenhirState36 | MenhirState42 | MenhirState81 | MenhirState79 | MenhirState44 | MenhirState45 | MenhirState50 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 | MenhirState61 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | COLON | ELSE | EOEX | EQUAL | IDENT _ | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))) = _menhir_stack in
            let _v : (Tfbsrxast.expr) = 
# 128 "src/TFbSRX/tfbsrxparser.mly"
      ( _1 )
# 1913 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | COLON | ELSE | EOEX | EQUAL | IDENT _ | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.expr))), _, (_2 : (Tfbsrxast.expr))) = _menhir_stack in
            let _v : (Tfbsrxast.expr) = 
# 130 "src/TFbSRX/tfbsrxparser.mly"
      ( Appl(_1,_2) )
# 1935 "src/TFbSRX/tfbsrxparser.ml"
             in
            _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | RCURLY ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "src/TFbSRX/tfbsrxparser.mly"
       (int)
# 2323 "src/TFbSRX/tfbsrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 43 "src/TFbSRX/tfbsrxparser.mly"
       (int)
# 2331 "src/TFbSRX/tfbsrxparser.ml"
    )) = _v in
    let _v : (Tfbsrxast.expr) = 
# 135 "src/TFbSRX/tfbsrxparser.mly"
      ( Int _1 )
# 2336 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 2380 "src/TFbSRX/tfbsrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 40 "src/TFbSRX/tfbsrxparser.mly"
       (string)
# 2388 "src/TFbSRX/tfbsrxparser.ml"
    )) = _v in
    let _v : (Tfbsrxast.ident) = 
# 168 "src/TFbSRX/tfbsrxparser.mly"
      ( Ident _1 )
# 2393 "src/TFbSRX/tfbsrxparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | INTTYPE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LCURLY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | INTTYPE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LCURLY ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState98 | MenhirState1 | MenhirState2 | MenhirState26 | MenhirState27 | MenhirState28 | MenhirState89 | MenhirState36 | MenhirState42 | MenhirState81 | MenhirState79 | MenhirState44 | MenhirState45 | MenhirState50 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Tfbsrxast.ident))) = _menhir_stack in
        let _v : (Tfbsrxast.expr) = 
# 163 "src/TFbSRX/tfbsrxparser.mly"
      ( Var _1 )
# 2471 "src/TFbSRX/tfbsrxparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Tfbsrxast.expr)) = _v in
        let _v : (Tfbsrxast.expr) = 
# 139 "src/TFbSRX/tfbsrxparser.mly"
      ( _1 )
# 2479 "src/TFbSRX/tfbsrxparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "src/TFbSRX/tfbsrxparser.mly"
       (bool)
# 2538 "src/TFbSRX/tfbsrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 32 "src/TFbSRX/tfbsrxparser.mly"
       (bool)
# 2546 "src/TFbSRX/tfbsrxparser.ml"
    )) = _v in
    let _v : (Tfbsrxast.expr) = 
# 137 "src/TFbSRX/tfbsrxparser.mly"
      ( Bool _1 )
# 2551 "src/TFbSRX/tfbsrxparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 85 "src/TFbSRX/tfbsrxparser.mly"
      (Tfbsrxast.expr)
# 2570 "src/TFbSRX/tfbsrxparser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FUNCTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GET ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LCURLY ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 199 "src/TFbSRX/tfbsrxparser.mly"
  

# 2618 "src/TFbSRX/tfbsrxparser.ml"

# 269 "<standard.mly>"
  

# 2623 "src/TFbSRX/tfbsrxparser.ml"
