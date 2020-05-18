
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | VARIANT of (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 12 "src/AFbV/afbvparser.ml"
  )
    | THEN
    | TAIL
    | STRING of (
# 22 "src/AFbV/afbvparser.mly"
       (string)
# 19 "src/AFbV/afbvparser.ml"
  )
    | SND
    | SENDTO
    | SEMICOLON
    | RPAREN
    | RBRACKET
    | PRINT
    | PLUS
    | PIPE
    | OR
    | NOT
    | MINUS
    | MATCH
    | LPAREN
    | LET
    | LBRACKET
    | INT of (
# 21 "src/AFbV/afbvparser.mly"
       (int)
# 39 "src/AFbV/afbvparser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 18 "src/AFbV/afbvparser.mly"
       (string)
# 46 "src/AFbV/afbvparser.ml"
  )
    | HEAD
    | GOESTO
    | FUNCTION
    | FST
    | EQUAL
    | EOEX
    | ELSE
    | CREATE
    | CONS
    | COMMA
    | BOOL of (
# 12 "src/AFbV/afbvparser.mly"
       (bool)
# 61 "src/AFbV/afbvparser.ml"
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
  | MenhirState81
  | MenhirState78
  | MenhirState76
  | MenhirState73
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState63
  | MenhirState58
  | MenhirState56
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState27
  | MenhirState21
  | MenhirState20
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "src/AFbV/afbvparser.mly"
  

open Afbvast;;


# 125 "src/AFbV/afbvparser.ml"

let rec _menhir_goto_pattern_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Afbvast.name * Afbvast.ident * Afbvast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PIPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PIPE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | VARIANT _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | AND | COMMA | CONS | ELSE | EOEX | EQUAL | IN | MINUS | OR | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))), _, (_4 : ((Afbvast.name * Afbvast.ident * Afbvast.expr) list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Afbvast.expr) = 
# 111 "src/AFbV/afbvparser.mly"
      ( Match(_2, _4) )
# 155 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAREN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> (Afbvast.name * Afbvast.ident * Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Afbvast.name * Afbvast.ident * Afbvast.expr)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : ((Afbvast.name * Afbvast.ident * Afbvast.expr) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((Afbvast.name * Afbvast.ident * Afbvast.expr) list) = 
# 170 "src/AFbV/afbvparser.mly"
      ( _1 @ [_3] )
# 192 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Afbvast.name * Afbvast.ident * Afbvast.expr)) = _v in
        let _v : ((Afbvast.name * Afbvast.ident * Afbvast.expr) list) = 
# 168 "src/AFbV/afbvparser.mly"
      ( [_1] )
# 202 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 211 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAREN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VARIANT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LPAREN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce14 : _menhir_env -> (('ttv_tail * _menhir_state * (Afbvast.expr))) * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Afbvast.expr) = 
# 103 "src/AFbV/afbvparser.mly"
      ( Pair(_1, _3) )
# 261 "src/AFbV/afbvparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_appl_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | AND | COMMA | CONS | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))) = _menhir_stack in
        let _v : (Afbvast.expr) = 
# 81 "src/AFbV/afbvparser.mly"
      ( _1 )
# 704 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
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
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 121 "src/AFbV/afbvparser.mly"
      ( Send(_1, _3) )
# 802 "src/AFbV/afbvparser.ml"
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
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 113 "src/AFbV/afbvparser.mly"
      ( Cons(_1, _3) )
# 825 "src/AFbV/afbvparser.ml"
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
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 99 "src/AFbV/afbvparser.mly"
      ( Seq(_1, _3) )
# 864 "src/AFbV/afbvparser.ml"
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
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 83 "src/AFbV/afbvparser.mly"
      ( Plus(_1, _3) )
# 887 "src/AFbV/afbvparser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 89 "src/AFbV/afbvparser.mly"
      ( Or(_1, _3) )
# 920 "src/AFbV/afbvparser.ml"
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
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 85 "src/AFbV/afbvparser.mly"
      ( Minus(_1, _3) )
# 943 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | OR | PIPE | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 93 "src/AFbV/afbvparser.mly"
      ( Equal(_1, _3) )
# 970 "src/AFbV/afbvparser.ml"
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
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOEX | IN | OR | PIPE | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))), _, (_3 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Afbvast.expr) = 
# 87 "src/AFbV/afbvparser.mly"
      ( And(_1, _3) )
# 1001 "src/AFbV/afbvparser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_3 : (Afbvast.expr))), _, (_5 : (Afbvast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 119 "src/AFbV/afbvparser.mly"
      ( Create(_3, _5) )
# 1064 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | COMMA | SEMICOLON | SENDTO ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.ident))), _, (_4 : (Afbvast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 95 "src/AFbV/afbvparser.mly"
      ( Function(_2, _4) )
# 1106 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 115 "src/AFbV/afbvparser.mly"
      ( Head _2 )
# 1129 "src/AFbV/afbvparser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))), _, (_4 : (Afbvast.expr))), _, (_6 : (Afbvast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 97 "src/AFbV/afbvparser.mly"
      ( If(_2, _4, _6) )
# 1316 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 147 "src/AFbV/afbvparser.mly"
      (
        let rec seq_to_list e = match e with
          | Seq(e1, e2) -> Cons(e1, seq_to_list e2)
          | v -> Cons(v, EmptyList)
        in
          seq_to_list _2
      )
# 1360 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.ident))), _, (_4 : (Afbvast.expr))), _, (_6 : (Afbvast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 101 "src/AFbV/afbvparser.mly"
      ( Let(_2, _4, _6) )
# 1479 "src/AFbV/afbvparser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PIPE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | VARIANT _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 1558 "src/AFbV/afbvparser.ml"
            ))), _, (_2 : (Afbvast.ident))), _, (_4 : (Afbvast.expr))) = _menhir_stack in
            let _3 = () in
            let _v : (Afbvast.name * Afbvast.ident * Afbvast.expr) = 
# 175 "src/AFbV/afbvparser.mly"
      ( ((Name _1), _2,_4) )
# 1564 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 1601 "src/AFbV/afbvparser.ml"
            ))), _, (_3 : (Afbvast.ident))), _, (_5 : (Afbvast.expr))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Afbvast.name * Afbvast.ident * Afbvast.expr) = 
# 177 "src/AFbV/afbvparser.mly"
      ( ((Name _2), _3, _5) )
# 1608 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 91 "src/AFbV/afbvparser.mly"
      ( Not _2 )
# 1631 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 123 "src/AFbV/afbvparser.mly"
      ( Print _2 )
# 1654 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 143 "src/AFbV/afbvparser.mly"
      ( _2 )
# 1692 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Afbvast.expr) = 
# 117 "src/AFbV/afbvparser.mly"
      ( Tail _2 )
# 1719 "src/AFbV/afbvparser.ml"
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
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SENDTO | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 1740 "src/AFbV/afbvparser.ml"
            ))), _, (_2 : (Afbvast.expr))) = _menhir_stack in
            let _v : (Afbvast.expr) = 
# 109 "src/AFbV/afbvparser.mly"
      ( Variant(Name _1, _2) )
# 1745 "src/AFbV/afbvparser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EOEX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 71 "src/AFbV/afbvparser.mly"
      (Afbvast.expr)
# 1773 "src/AFbV/afbvparser.ml"
            ) = 
# 76 "src/AFbV/afbvparser.mly"
            ( _1 )
# 1777 "src/AFbV/afbvparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 71 "src/AFbV/afbvparser.mly"
      (Afbvast.expr)
# 1784 "src/AFbV/afbvparser.ml"
            )) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SENDTO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_pattern_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Afbvast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Afbvast.ident))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Afbvast.ident) = 
# 184 "src/AFbV/afbvparser.mly"
       ( _2 )
# 1832 "src/AFbV/afbvparser.ml"
             in
            _menhir_goto_pattern_ident _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Afbvast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Afbvast.expr)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Afbvast.expr) = 
# 105 "src/AFbV/afbvparser.mly"
      ( Fst _2 )
# 1968 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState1 | MenhirState2 | MenhirState5 | MenhirState6 | MenhirState7 | MenhirState78 | MenhirState73 | MenhirState8 | MenhirState63 | MenhirState12 | MenhirState13 | MenhirState58 | MenhirState56 | MenhirState16 | MenhirState17 | MenhirState20 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState32 | MenhirState30 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Afbvast.expr)) = _v in
        let _v : (Afbvast.expr) = 
# 128 "src/AFbV/afbvparser.mly"
      ( _1 )
# 1978 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Afbvast.expr)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Afbvast.expr))) = _menhir_stack in
        let _v : (Afbvast.expr) = 
# 130 "src/AFbV/afbvparser.mly"
      ( Appl(_1,_2) )
# 1989 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Afbvast.expr)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Afbvast.expr) = 
# 107 "src/AFbV/afbvparser.mly"
      ( Snd _2 )
# 2001 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "src/AFbV/afbvparser.mly"
       (string)
# 2165 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
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
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/AFbV/afbvparser.mly"
       (string)
# 2263 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 22 "src/AFbV/afbvparser.mly"
       (string)
# 2271 "src/AFbV/afbvparser.ml"
    )) = _v in
    let _v : (Afbvast.expr) = 
# 139 "src/AFbV/afbvparser.mly"
      ( String _1 )
# 2276 "src/AFbV/afbvparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
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
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState13 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Afbvast.expr) = 
# 145 "src/AFbV/afbvparser.mly"
      ( EmptyList )
# 2549 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "src/AFbV/afbvparser.mly"
       (int)
# 2568 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 21 "src/AFbV/afbvparser.mly"
       (int)
# 2576 "src/AFbV/afbvparser.ml"
    )) = _v in
    let _v : (Afbvast.expr) = 
# 135 "src/AFbV/afbvparser.mly"
      ( Int _1 )
# 2581 "src/AFbV/afbvparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/AFbV/afbvparser.mly"
       (string)
# 2635 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 18 "src/AFbV/afbvparser.mly"
       (string)
# 2643 "src/AFbV/afbvparser.ml"
    )) = _v in
    let _v : (Afbvast.ident) = 
# 163 "src/AFbV/afbvparser.mly"
      ( Ident _1 )
# 2648 "src/AFbV/afbvparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | CREATE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | FST ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | FUNCTION ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | HEAD ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | IF ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | LBRACKET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | LET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | MATCH ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | NOT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | SND ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | TAIL ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | VARIANT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState1 | MenhirState2 | MenhirState4 | MenhirState5 | MenhirState6 | MenhirState7 | MenhirState78 | MenhirState73 | MenhirState8 | MenhirState63 | MenhirState12 | MenhirState13 | MenhirState58 | MenhirState56 | MenhirState16 | MenhirState17 | MenhirState20 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState27 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Afbvast.ident))) = _menhir_stack in
        let _v : (Afbvast.expr) = 
# 158 "src/AFbV/afbvparser.mly"
      ( Var _1 )
# 2771 "src/AFbV/afbvparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Afbvast.expr)) = _v in
        let _v : (Afbvast.expr) = 
# 141 "src/AFbV/afbvparser.mly"
      ( _1 )
# 2779 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState76 | MenhirState67 | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Afbvast.ident))) = _menhir_stack in
        let _v : (Afbvast.ident) = 
# 182 "src/AFbV/afbvparser.mly"
       ( _1 )
# 2789 "src/AFbV/afbvparser.ml"
         in
        _menhir_goto_pattern_ident _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CREATE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | FST ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | FUNCTION ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | HEAD ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | INT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LBRACKET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | MATCH ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NOT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | SND ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | TAIL ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | VARIANT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "src/AFbV/afbvparser.mly"
       (bool)
# 2939 "src/AFbV/afbvparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "src/AFbV/afbvparser.mly"
       (bool)
# 2947 "src/AFbV/afbvparser.ml"
    )) = _v in
    let _v : (Afbvast.expr) = 
# 137 "src/AFbV/afbvparser.mly"
      ( Bool _1 )
# 2952 "src/AFbV/afbvparser.ml"
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
# 71 "src/AFbV/afbvparser.mly"
      (Afbvast.expr)
# 2971 "src/AFbV/afbvparser.ml"
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
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | CREATE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FST ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUNCTION ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HEAD ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACKET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MATCH ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SND ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TAIL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VARIANT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 187 "src/AFbV/afbvparser.mly"
  

# 3029 "src/AFbV/afbvparser.ml"

# 269 "<standard.mly>"
  

# 3034 "src/AFbV/afbvparser.ml"
