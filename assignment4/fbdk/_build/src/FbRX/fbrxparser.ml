
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | TRY
    | THEN
    | STRING of (
# 21 "src/FbRX/fbrxparser.mly"
       (string)
# 14 "src/FbRX/fbrxparser.ml"
  )
    | SEMICOLON
    | RPAREN
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
    | INT of (
# 23 "src/FbRX/fbrxparser.mly"
       (int)
# 31 "src/FbRX/fbrxparser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 38 "src/FbRX/fbrxparser.ml"
  )
    | GOESTO
    | FUNCTION
    | EXN of (
# 56 "src/FbRX/fbrxparser.mly"
       (string)
# 45 "src/FbRX/fbrxparser.ml"
  )
    | EQUAL
    | EOEX
    | ELSE
    | DOT
    | BOOL of (
# 22 "src/FbRX/fbrxparser.mly"
       (bool)
# 54 "src/FbRX/fbrxparser.ml"
  )
    | APPEND
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
  | MenhirState74
  | MenhirState71
  | MenhirState64
  | MenhirState62
  | MenhirState59
  | MenhirState56
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState31
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState23
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState4
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "src/FbRX/fbrxparser.mly"
  

open Fbrxast;;
exception DuplicateLabel

let mkexn n =
  "#" ^ n

# 116 "src/FbRX/fbrxparser.ml"

let rec _menhir_goto_record_body : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Fbrxast.label * Fbrxast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : ((Fbrxast.label * Fbrxast.expr) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 132 "src/FbRX/fbrxparser.mly"
      ( Record _2 )
# 137 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Fbrxast.label))), _, (_3 : (Fbrxast.expr))), _, (_5 : ((Fbrxast.label * Fbrxast.expr) list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Fbrxast.label * Fbrxast.expr) list) = 
# 146 "src/FbRX/fbrxparser.mly"
      ( let rec addifnotpresent (lab, e) l =
          match l with [] -> [(lab,e)]
	   | (l1,e1) :: tl -> if l1=lab then (raise DuplicateLabel) else (l1,e1) :: (addifnotpresent (lab, e) tl)
          in addifnotpresent (_1, _3) _5 )
# 158 "src/FbRX/fbrxparser.ml"
         in
        _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.ident))), _, (_4 : (Fbrxast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 102 "src/FbRX/fbrxparser.mly"
      ( Function(_2, _4) )
# 397 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 88 "src/FbRX/fbrxparser.mly"
      ( Plus(_1, _3) )
# 420 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 100 "src/FbRX/fbrxparser.mly"
      ( Append(_1, _3) )
# 453 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 94 "src/FbRX/fbrxparser.mly"
      ( Or(_1, _3) )
# 486 "src/FbRX/fbrxparser.ml"
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
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 90 "src/FbRX/fbrxparser.mly"
      ( Minus(_1, _3) )
# 509 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | OR | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 98 "src/FbRX/fbrxparser.mly"
      ( Equal(_1, _3) )
# 536 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | OR | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrxast.expr) = 
# 92 "src/FbRX/fbrxparser.mly"
      ( And(_1, _3) )
# 567 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.expr))), _, (_4 : (Fbrxast.expr))), _, (_6 : (Fbrxast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 104 "src/FbRX/fbrxparser.mly"
      ( If(_2, _4, _6) )
# 714 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.label))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : ((Fbrxast.label * Fbrxast.expr) list) = 
# 144 "src/FbRX/fbrxparser.mly"
      ( [(_1, _3)] )
# 758 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _), _, (_3 : (Fbrxast.ident))), _, (_4 : (Fbrxast.ident))), _, (_6 : (Fbrxast.expr))), _, (_8 : (Fbrxast.expr))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 106 "src/FbRX/fbrxparser.mly"
      ( LetRec(_3, _4, _6, _8) )
# 850 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.ident))), _, (_4 : (Fbrxast.expr))), _, (_6 : (Fbrxast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 108 "src/FbRX/fbrxparser.mly"
      ( Let(_2, _4, _6) )
# 941 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 139 "src/FbRX/fbrxparser.mly"
      ( _2 )
# 977 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 96 "src/FbRX/fbrxparser.mly"
      ( Not _2 )
# 1000 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (string))), _, (_3 : (Fbrxast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 112 "src/FbRX/fbrxparser.mly"
      ( let n = _2 in Raise(mkexn n, _3) )
# 1033 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXN _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (_2 : (Fbrxast.expr))), _, (_4 : (string))), (_5 : (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 1100 "src/FbRX/fbrxparser.ml"
            ))), _, (_7 : (Fbrxast.expr))) = _menhir_stack in
            let _6 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrxast.expr) = 
# 110 "src/FbRX/fbrxparser.mly"
      ( let n = _4 in Try(_2, mkexn n, Ident _5, _7) )
# 1108 "src/FbRX/fbrxparser.ml"
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | APPEND ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | EOEX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "src/FbRX/fbrxparser.mly"
      (Fbrxast.expr)
# 1134 "src/FbRX/fbrxparser.ml"
            ) = 
# 81 "src/FbRX/fbrxparser.mly"
            ( _1 )
# 1138 "src/FbRX/fbrxparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 76 "src/FbRX/fbrxparser.mly"
      (Fbrxast.expr)
# 1145 "src/FbRX/fbrxparser.ml"
            )) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exn_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            let ((_menhir_stack, _menhir_s), _, (_2 : (string))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string) = 
# 170 "src/FbRX/fbrxparser.mly"
       ( _2 )
# 1184 "src/FbRX/fbrxparser.ml"
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
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | FUNCTION ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LCURLY ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | LET ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | LPAREN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | NOT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | RAISE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | STRING _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | TRY ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | MenhirState71 ->
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
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
                | FUNCTION ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | IDENT _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
                | IF ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | INT _v ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
                | LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | LET ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | LPAREN ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | NOT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | RAISE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | STRING _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
                | TRY ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
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

and _menhir_goto_appl_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | AND | APPEND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))) = _menhir_stack in
        let _v : (Fbrxast.expr) = 
# 86 "src/FbRX/fbrxparser.mly"
      ( _1 )
# 1310 "src/FbRX/fbrxparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

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
# 56 "src/FbRX/fbrxparser.mly"
       (string)
# 1348 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 56 "src/FbRX/fbrxparser.mly"
       (string)
# 1356 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (string) = 
# 168 "src/FbRX/fbrxparser.mly"
       ( _1 )
# 1361 "src/FbRX/fbrxparser.ml"
     in
    _menhir_goto_exn_def _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 1368 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 1376 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (Fbrxast.label) = 
# 154 "src/FbRX/fbrxparser.mly"
      ( Lab _1 )
# 1381 "src/FbRX/fbrxparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 | MenhirState17 ->
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
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_3 : (Fbrxast.label))) = _menhir_stack in
        let _2 = () in
        let _v : (Fbrxast.expr) = 
# 136 "src/FbRX/fbrxparser.mly"
      ( Select(_3, _1) )
# 1437 "src/FbRX/fbrxparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrxast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState74 | MenhirState1 | MenhirState8 | MenhirState9 | MenhirState10 | MenhirState64 | MenhirState62 | MenhirState59 | MenhirState16 | MenhirState23 | MenhirState53 | MenhirState51 | MenhirState25 | MenhirState48 | MenhirState46 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | BOOL _ | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | STRING _ | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))) = _menhir_stack in
            let _v : (Fbrxast.expr) = 
# 117 "src/FbRX/fbrxparser.mly"
      ( _1 )
# 1465 "src/FbRX/fbrxparser.ml"
             in
            _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | BOOL _ | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMICOLON | STRING _ | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrxast.expr))), _, (_2 : (Fbrxast.expr))) = _menhir_stack in
            let _v : (Fbrxast.expr) = 
# 119 "src/FbRX/fbrxparser.mly"
      ( Appl(_1,_2) )
# 1487 "src/FbRX/fbrxparser.ml"
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
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
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
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "src/FbRX/fbrxparser.mly"
       (string)
# 1668 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 21 "src/FbRX/fbrxparser.mly"
       (string)
# 1676 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (Fbrxast.expr) = 
# 128 "src/FbRX/fbrxparser.mly"
      ( String _1 )
# 1681 "src/FbRX/fbrxparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

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

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState11 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | RCURLY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState17 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Fbrxast.expr) = 
# 134 "src/FbRX/fbrxparser.mly"
      ( Record [] )
# 1815 "src/FbRX/fbrxparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/FbRX/fbrxparser.mly"
       (int)
# 1826 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 23 "src/FbRX/fbrxparser.mly"
       (int)
# 1834 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (Fbrxast.expr) = 
# 124 "src/FbRX/fbrxparser.mly"
      ( Int _1 )
# 1839 "src/FbRX/fbrxparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 1881 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 18 "src/FbRX/fbrxparser.mly"
       (string)
# 1889 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (Fbrxast.ident) = 
# 163 "src/FbRX/fbrxparser.mly"
      ( Ident _1 )
# 1894 "src/FbRX/fbrxparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | MenhirState14 ->
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
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
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
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState74 | MenhirState1 | MenhirState8 | MenhirState9 | MenhirState10 | MenhirState64 | MenhirState62 | MenhirState59 | MenhirState16 | MenhirState23 | MenhirState53 | MenhirState51 | MenhirState25 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbrxast.ident))) = _menhir_stack in
        let _v : (Fbrxast.expr) = 
# 158 "src/FbRX/fbrxparser.mly"
      ( Var _1 )
# 2004 "src/FbRX/fbrxparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Fbrxast.expr)) = _v in
        let _v : (Fbrxast.expr) = 
# 130 "src/FbRX/fbrxparser.mly"
      ( _1 )
# 2012 "src/FbRX/fbrxparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState11 ->
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
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | FUNCTION ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LET ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LPAREN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NOT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | RAISE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | TRY ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/FbRX/fbrxparser.mly"
       (bool)
# 2078 "src/FbRX/fbrxparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 22 "src/FbRX/fbrxparser.mly"
       (bool)
# 2086 "src/FbRX/fbrxparser.ml"
    )) = _v in
    let _v : (Fbrxast.expr) = 
# 126 "src/FbRX/fbrxparser.mly"
      ( Bool _1 )
# 2091 "src/FbRX/fbrxparser.ml"
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
# 76 "src/FbRX/fbrxparser.mly"
      (Fbrxast.expr)
# 2110 "src/FbRX/fbrxparser.ml"
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
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RAISE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRY ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 171 "src/FbRX/fbrxparser.mly"
  

# 2156 "src/FbRX/fbrxparser.ml"

# 269 "<standard.mly>"
  

# 2161 "src/FbRX/fbrxparser.ml"
