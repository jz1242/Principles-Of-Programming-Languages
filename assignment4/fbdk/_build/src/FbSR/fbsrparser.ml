
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | THEN
    | SET
    | SEMI
    | RPAREN
    | REF
    | REC
    | RCURLY
    | PLUS
    | OR
    | NOT
    | MINUS
    | LPAREN
    | LET
    | LCURLY
    | INT of (
# 23 "src/FbSR/fbsrparser.mly"
       (int)
# 25 "src/FbSR/fbsrparser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 20 "src/FbSR/fbsrparser.mly"
       (string)
# 32 "src/FbSR/fbsrparser.ml"
  )
    | GOESTO
    | GET
    | FUNCTION
    | EQUAL
    | EOEX
    | ELSE
    | DOT
    | BOOL of (
# 12 "src/FbSR/fbsrparser.mly"
       (bool)
# 44 "src/FbSR/fbsrparser.ml"
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
  | MenhirState59
  | MenhirState57
  | MenhirState54
  | MenhirState51
  | MenhirState48
  | MenhirState46
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState25
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState16
  | MenhirState10
  | MenhirState9
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "src/FbSR/fbsrparser.mly"
  

open Fbsrast


# 98 "src/FbSR/fbsrparser.ml"

let rec _menhir_goto_record_body : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Fbsrast.label * Fbsrast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : ((Fbsrast.label * Fbsrast.expr) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 110 "src/FbSR/fbsrparser.mly"
      ( Record _2 )
# 119 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Fbsrast.label))), _, (_3 : (Fbsrast.expr))), _, (_5 : ((Fbsrast.label * Fbsrast.expr) list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Fbsrast.label * Fbsrast.expr) list) = 
# 123 "src/FbSR/fbsrparser.mly"
      ( (_1, _3)::_5 )
# 137 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.ident))), _, (_4 : (Fbsrast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 80 "src/FbSR/fbsrparser.mly"
      ( Function(_2, _4) )
# 364 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbsrast.expr) = 
# 90 "src/FbSR/fbsrparser.mly"
      ( Set(_1, _3) )
# 397 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Fbsrast.expr) = 
# 68 "src/FbSR/fbsrparser.mly"
      ( Plus(_1, _3) )
# 414 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | SET | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbsrast.expr) = 
# 74 "src/FbSR/fbsrparser.mly"
      ( Or(_1, _3) )
# 439 "src/FbSR/fbsrparser.ml"
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Fbsrast.expr) = 
# 70 "src/FbSR/fbsrparser.mly"
      ( Minus(_1, _3) )
# 456 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | OR | RCURLY | RPAREN | SEMI | SET | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbsrast.expr) = 
# 78 "src/FbSR/fbsrparser.mly"
      ( Equal(_1, _3) )
# 475 "src/FbSR/fbsrparser.ml"
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
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | OR | RCURLY | RPAREN | SEMI | SET | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbsrast.expr) = 
# 72 "src/FbSR/fbsrparser.mly"
      ( And(_1, _3) )
# 504 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Fbsrast.expr) = 
# 92 "src/FbSR/fbsrparser.mly"
      ( Get _2 )
# 521 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
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
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.expr))), _, (_4 : (Fbsrast.expr))), _, (_6 : (Fbsrast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 84 "src/FbSR/fbsrparser.mly"
      ( If(_2, _4, _6) )
# 658 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.label))), _, (_3 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : ((Fbsrast.label * Fbsrast.expr) list) = 
# 121 "src/FbSR/fbsrparser.mly"
      ( [(_1, _3)] )
# 702 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _), _, (_3 : (Fbsrast.ident))), _, (_4 : (Fbsrast.ident))), _, (_6 : (Fbsrast.expr))), _, (_8 : (Fbsrast.expr))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 82 "src/FbSR/fbsrparser.mly"
      ( LetRec(_3, _4, _6, _8) )
# 792 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | RCURLY | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.ident))), _, (_4 : (Fbsrast.expr))), _, (_6 : (Fbsrast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 86 "src/FbSR/fbsrparser.mly"
      ( Let(_2, _4, _6) )
# 881 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbsrast.expr) = 
# 116 "src/FbSR/fbsrparser.mly"
      ( _2 )
# 915 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Fbsrast.expr) = 
# 76 "src/FbSR/fbsrparser.mly"
      ( Not _2 )
# 934 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Fbsrast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Fbsrast.expr) = 
# 88 "src/FbSR/fbsrparser.mly"
      ( Ref _2 )
# 945 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EOEX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "src/FbSR/fbsrparser.mly"
      (Fbsrast.expr)
# 963 "src/FbSR/fbsrparser.ml"
            ) = 
# 61 "src/FbSR/fbsrparser.mly"
            ( _1 )
# 967 "src/FbSR/fbsrparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 56 "src/FbSR/fbsrparser.mly"
      (Fbsrast.expr)
# 974 "src/FbSR/fbsrparser.ml"
            )) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_appl_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))) = _menhir_stack in
        let _v : (Fbsrast.expr) = 
# 66 "src/FbSR/fbsrparser.mly"
      ( _1 )
# 1019 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/FbSR/fbsrparser.mly"
       (string)
# 1042 "src/FbSR/fbsrparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 20 "src/FbSR/fbsrparser.mly"
       (string)
# 1050 "src/FbSR/fbsrparser.ml"
    )) = _v in
    let _v : (Fbsrast.label) = 
# 128 "src/FbSR/fbsrparser.mly"
      ( Lab _1 )
# 1055 "src/FbSR/fbsrparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState51 | MenhirState10 ->
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
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | REF ->
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
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_3 : (Fbsrast.label))) = _menhir_stack in
        let _2 = () in
        let _v : (Fbsrast.expr) = 
# 114 "src/FbSR/fbsrparser.mly"
      ( Select(_3, _1) )
# 1109 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbsrast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState1 | MenhirState2 | MenhirState3 | MenhirState59 | MenhirState57 | MenhirState54 | MenhirState9 | MenhirState16 | MenhirState48 | MenhirState46 | MenhirState18 | MenhirState19 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState32 | MenhirState30 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))) = _menhir_stack in
            let _v : (Fbsrast.expr) = 
# 97 "src/FbSR/fbsrparser.mly"
      ( _1 )
# 1137 "src/FbSR/fbsrparser.ml"
             in
            _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
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
        | DOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | RPAREN | SEMI | SET | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbsrast.expr))), _, (_2 : (Fbsrast.expr))) = _menhir_stack in
            let _v : (Fbsrast.expr) = 
# 99 "src/FbSR/fbsrparser.mly"
      ( Appl(_1,_2) )
# 1159 "src/FbSR/fbsrparser.ml"
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
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | REF ->
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
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | REF ->
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
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState4 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | RCURLY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState10 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Fbsrast.expr) = 
# 112 "src/FbSR/fbsrparser.mly"
      ( Record [] )
# 1430 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/FbSR/fbsrparser.mly"
       (int)
# 1441 "src/FbSR/fbsrparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 23 "src/FbSR/fbsrparser.mly"
       (int)
# 1449 "src/FbSR/fbsrparser.ml"
    )) = _v in
    let _v : (Fbsrast.expr) = 
# 104 "src/FbSR/fbsrparser.mly"
      ( Int _1 )
# 1454 "src/FbSR/fbsrparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/FbSR/fbsrparser.mly"
       (string)
# 1494 "src/FbSR/fbsrparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 20 "src/FbSR/fbsrparser.mly"
       (string)
# 1502 "src/FbSR/fbsrparser.ml"
    )) = _v in
    let _v : (Fbsrast.ident) = 
# 137 "src/FbSR/fbsrparser.mly"
      ( Ident _1 )
# 1507 "src/FbSR/fbsrparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | MenhirState7 ->
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
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
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
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState1 | MenhirState2 | MenhirState3 | MenhirState59 | MenhirState57 | MenhirState54 | MenhirState9 | MenhirState16 | MenhirState48 | MenhirState46 | MenhirState18 | MenhirState19 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbsrast.ident))) = _menhir_stack in
        let _v : (Fbsrast.expr) = 
# 132 "src/FbSR/fbsrparser.mly"
      ( Var _1 )
# 1613 "src/FbSR/fbsrparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Fbsrast.expr)) = _v in
        let _v : (Fbsrast.expr) = 
# 108 "src/FbSR/fbsrparser.mly"
      ( _1 )
# 1621 "src/FbSR/fbsrparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
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
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | FUNCTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | GET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NOT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | REF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "src/FbSR/fbsrparser.mly"
       (bool)
# 1718 "src/FbSR/fbsrparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "src/FbSR/fbsrparser.mly"
       (bool)
# 1726 "src/FbSR/fbsrparser.ml"
    )) = _v in
    let _v : (Fbsrast.expr) = 
# 106 "src/FbSR/fbsrparser.mly"
      ( Bool _1 )
# 1731 "src/FbSR/fbsrparser.ml"
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
# 56 "src/FbSR/fbsrparser.mly"
      (Fbsrast.expr)
# 1750 "src/FbSR/fbsrparser.ml"
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
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FUNCTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 140 "src/FbSR/fbsrparser.mly"
  

# 1794 "src/FbSR/fbsrparser.ml"

# 269 "<standard.mly>"
  

# 1799 "src/FbSR/fbsrparser.ml"
