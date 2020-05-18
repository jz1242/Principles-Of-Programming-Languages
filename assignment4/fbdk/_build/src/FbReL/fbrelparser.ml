
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | THEN
    | SEMICOLON
    | RPAREN
    | RCURLY
    | RBRACKET
    | PLUS
    | PIPE
    | OR
    | NOT
    | MINUS
    | MATCH
    | LPAREN
    | LET
    | LCURLY
    | LBRACKET
    | INT of (
# 23 "src/FbReL/fbrelparser.mly"
       (int)
# 27 "src/FbReL/fbrelparser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 34 "src/FbReL/fbrelparser.ml"
  )
    | GOESTO
    | FUNCTION
    | EQUAL
    | EOEX
    | ELSE
    | DOT
    | CONS
    | BOOL of (
# 13 "src/FbReL/fbrelparser.mly"
       (bool)
# 46 "src/FbReL/fbrelparser.ml"
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
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState57
  | MenhirState53
  | MenhirState48
  | MenhirState46
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState24
  | MenhirState21
  | MenhirState19
  | MenhirState18
  | MenhirState15
  | MenhirState14
  | MenhirState8
  | MenhirState7
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "src/FbReL/fbrelparser.mly"
  

open Fbrelast;;
exception DuplicateLabel


# 102 "src/FbReL/fbrelparser.ml"

let rec _menhir_goto_record_body : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Fbrelast.label * Fbrelast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : ((Fbrelast.label * Fbrelast.expr) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 124 "src/FbReL/fbrelparser.mly"
      ( Record _2 )
# 123 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Fbrelast.label))), _, (_3 : (Fbrelast.expr))), _, (_5 : ((Fbrelast.label * Fbrelast.expr) list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Fbrelast.label * Fbrelast.expr) list) = 
# 138 "src/FbReL/fbrelparser.mly"
      ( let rec addifnotpresent (lab, e) l =
          match l with [] -> [(lab,e)]
	   | (l1,e1) :: tl -> if l1=lab then (raise DuplicateLabel) else (l1,e1) :: (addifnotpresent (lab, e) tl)
          in addifnotpresent (_1, _3) _5 )
# 144 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce20 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 153 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 159 "src/FbReL/fbrelparser.ml"
    ))) = _menhir_stack in
    let _v : (Fbrelast.label) = 
# 146 "src/FbReL/fbrelparser.mly"
      ( Lab _1 )
# 164 "src/FbReL/fbrelparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 | MenhirState8 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.label))) = _menhir_stack in
        let _2 = () in
        let _v : (Fbrelast.expr) = 
# 128 "src/FbReL/fbrelparser.mly"
      ( Select(_3, _1) )
# 218 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_appl_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | AND | CONS | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))) = _menhir_stack in
        let _v : (Fbrelast.expr) = 
# 71 "src/FbReL/fbrelparser.mly"
      ( _1 )
# 476 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 499 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.ident))), _, (_4 : (Fbrelast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 85 "src/FbReL/fbrelparser.mly"
      ( Function(_2, _4) )
# 537 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 104 "src/FbReL/fbrelparser.mly"
      ( Cons(_1, _3) )
# 572 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 73 "src/FbReL/fbrelparser.mly"
      ( Plus(_1, _3) )
# 595 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 106 "src/FbReL/fbrelparser.mly"
      ( Cons(_1, _3) )
# 618 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 79 "src/FbReL/fbrelparser.mly"
      ( Or(_1, _3) )
# 651 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 75 "src/FbReL/fbrelparser.mly"
      ( Minus(_1, _3) )
# 674 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | OR | PIPE | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 83 "src/FbReL/fbrelparser.mly"
      ( Equal(_1, _3) )
# 701 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | OR | PIPE | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Fbrelast.expr) = 
# 77 "src/FbReL/fbrelparser.mly"
      ( And(_1, _3) )
# 732 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NOT ->
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.expr))), _, (_4 : (Fbrelast.expr))), _, (_6 : (Fbrelast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 87 "src/FbReL/fbrelparser.mly"
      ( If(_2, _4, _6) )
# 879 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 96 "src/FbReL/fbrelparser.mly"
      (
        let rec seq_to_list e = match e with
          | Cons(e1, e2) -> Cons(e1, seq_to_list e2)
          | v -> Cons(v, EmptyList)
        in
          seq_to_list _2
      )
# 921 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState53 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQUAL ->
                    _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
                | AND | BOOL _ | CONS | DOT | IDENT _ | INT _ | LCURLY | LPAREN | MINUS | OR | PLUS | RCURLY | SEMICOLON ->
                    _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.label))), _, (_3 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : ((Fbrelast.label * Fbrelast.expr) list) = 
# 136 "src/FbReL/fbrelparser.mly"
      ( [(_1, _3)] )
# 1002 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_record_body _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.ident))), _, (_4 : (Fbrelast.expr))), _, (_6 : (Fbrelast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 89 "src/FbReL/fbrelparser.mly"
      ( Let(_2, _4, _6) )
# 1095 "src/FbReL/fbrelparser.ml"
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 131 "src/FbReL/fbrelparser.mly"
      ( _2 )
# 1131 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | GOESTO ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOL _v ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                        | FUNCTION ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | IDENT _v ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                        | IF ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | INT _v ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                        | LBRACKET ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | LCURLY ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | LET ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | LPAREN ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | MATCH ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | NOT ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOEX | IN | PIPE | RBRACKET | RCURLY | RPAREN | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.expr))), _, (_7 : (Fbrelast.expr))), _, (_9 : (Fbrelast.ident))), _, (_11 : (Fbrelast.ident))), _, (_13 : (Fbrelast.expr))) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 92 "src/FbReL/fbrelparser.mly"
      ( Match(_2, _7, _9, _11, _13) )
# 1300 "src/FbReL/fbrelparser.ml"
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
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | EOEX | EQUAL | IN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Fbrelast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Fbrelast.expr) = 
# 81 "src/FbReL/fbrelparser.mly"
      ( Not _2 )
# 1323 "src/FbReL/fbrelparser.ml"
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EOEX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 61 "src/FbReL/fbrelparser.mly"
      (Fbrelast.expr)
# 1349 "src/FbReL/fbrelparser.ml"
            ) = 
# 66 "src/FbReL/fbrelparser.mly"
            ( _1 )
# 1353 "src/FbReL/fbrelparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 61 "src/FbReL/fbrelparser.mly"
      (Fbrelast.expr)
# 1360 "src/FbReL/fbrelparser.ml"
            )) = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 1385 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 1391 "src/FbReL/fbrelparser.ml"
    ))) = _menhir_stack in
    let _v : (Fbrelast.ident) = 
# 155 "src/FbReL/fbrelparser.mly"
      ( Ident _1 )
# 1396 "src/FbReL/fbrelparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState1 | MenhirState71 | MenhirState65 | MenhirState2 | MenhirState3 | MenhirState57 | MenhirState7 | MenhirState53 | MenhirState14 | MenhirState15 | MenhirState48 | MenhirState46 | MenhirState18 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Fbrelast.ident))) = _menhir_stack in
        let _v : (Fbrelast.expr) = 
# 150 "src/FbReL/fbrelparser.mly"
      ( Var _1 )
# 1491 "src/FbReL/fbrelparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Fbrelast.expr)) = _v in
        let _v : (Fbrelast.expr) = 
# 122 "src/FbReL/fbrelparser.mly"
      ( _1 )
# 1499 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
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
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | FUNCTION ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | IDENT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LBRACKET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | LCURLY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | MATCH ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NOT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Fbrelast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState1 | MenhirState71 | MenhirState65 | MenhirState2 | MenhirState3 | MenhirState57 | MenhirState7 | MenhirState53 | MenhirState14 | MenhirState15 | MenhirState48 | MenhirState46 | MenhirState18 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CONS | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))) = _menhir_stack in
            let _v : (Fbrelast.expr) = 
# 111 "src/FbReL/fbrelparser.mly"
      ( _1 )
# 1586 "src/FbReL/fbrelparser.ml"
             in
            _menhir_goto_appl_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CONS | ELSE | EOEX | EQUAL | IDENT _ | IN | INT _ | LCURLY | LPAREN | MINUS | OR | PIPE | PLUS | RBRACKET | RCURLY | RPAREN | SEMICOLON | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Fbrelast.expr))), _, (_2 : (Fbrelast.expr))) = _menhir_stack in
            let _v : (Fbrelast.expr) = 
# 113 "src/FbReL/fbrelparser.mly"
      ( Appl(_1,_2) )
# 1608 "src/FbReL/fbrelparser.ml"
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
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
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
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
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NOT ->
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
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | RCURLY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Fbrelast.expr) = 
# 126 "src/FbReL/fbrelparser.mly"
      ( Record [] )
# 1870 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState15 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Fbrelast.expr) = 
# 94 "src/FbReL/fbrelparser.mly"
      ( EmptyList )
# 1917 "src/FbReL/fbrelparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/FbReL/fbrelparser.mly"
       (int)
# 1928 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 23 "src/FbReL/fbrelparser.mly"
       (int)
# 1936 "src/FbReL/fbrelparser.ml"
    )) = _v in
    let _v : (Fbrelast.expr) = 
# 118 "src/FbReL/fbrelparser.mly"
      ( Int _1 )
# 1941 "src/FbReL/fbrelparser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/FbReL/fbrelparser.mly"
       (string)
# 1981 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/FbReL/fbrelparser.mly"
       (bool)
# 2004 "src/FbReL/fbrelparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 13 "src/FbReL/fbrelparser.mly"
       (bool)
# 2012 "src/FbReL/fbrelparser.ml"
    )) = _v in
    let _v : (Fbrelast.expr) = 
# 120 "src/FbReL/fbrelparser.mly"
      ( Bool _1 )
# 2017 "src/FbReL/fbrelparser.ml"
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
# 61 "src/FbReL/fbrelparser.mly"
      (Fbrelast.expr)
# 2036 "src/FbReL/fbrelparser.ml"
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
    | FUNCTION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACKET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCURLY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MATCH ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 159 "src/FbReL/fbrelparser.mly"
  

# 2080 "src/FbReL/fbrelparser.ml"

# 269 "<standard.mly>"
  

# 2085 "src/FbReL/fbrelparser.ml"
