# 1 "src/FbSR/fbsrlexer.mll"
 
  open Fbsrparser;;

# 6 "src/FbSR/fbsrlexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\224\255\085\000\037\000\227\255\228\255\230\255\231\255\
    \232\255\234\255\236\255\001\000\000\000\002\000\025\000\026\000\
    \020\000\104\000\107\000\032\000\019\000\017\000\022\000\002\000\
    \000\000\002\000\006\000\255\255\043\000\253\255\252\255\028\000\
    \251\255\037\000\036\000\048\000\032\000\071\000\066\000\068\000\
    \249\255\064\000\080\000\239\255\245\255\248\255\093\000\110\000\
    \102\000\247\255\112\000\238\255\099\000\114\000\246\255\117\000\
    \242\255\244\255\104\000\243\255\241\255\240\255\237\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\030\000\029\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\026\000\020\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \022\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\255\255\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\025\000\025\000\000\000\255\255\000\000\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\023\000\023\000\023\000\000\000\023\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\004\000\023\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\008\000\025\000\010\000\026\000\012\000\005\000\027\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\013\000\011\000\062\000\009\000\061\000\060\000\
    \000\000\022\000\000\000\000\000\000\000\016\000\019\000\000\000\
    \000\000\018\000\000\000\000\000\014\000\000\000\020\000\021\000\
    \000\000\000\000\015\000\000\000\017\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
    \000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\007\000\002\000\006\000\058\000\055\000\
    \052\000\033\000\031\000\030\000\028\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\029\000\
    \032\000\041\000\035\000\036\000\037\000\034\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \038\000\039\000\040\000\042\000\002\000\043\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \047\000\045\000\050\000\048\000\049\000\051\000\053\000\054\000\
    \056\000\044\000\046\000\057\000\059\000\000\000\000\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \001\000\000\000\255\255\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\000\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\000\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\023\000\023\000\000\000\255\255\023\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\023\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\024\000\000\000\025\000\000\000\000\000\026\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\011\000\000\000\012\000\013\000\
    \255\255\000\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\000\000\
    \255\255\255\255\000\000\255\255\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\002\000\000\000\014\000\015\000\
    \016\000\019\000\020\000\021\000\022\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\028\000\
    \031\000\033\000\034\000\035\000\036\000\019\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \037\000\038\000\039\000\041\000\002\000\042\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \017\000\018\000\046\000\047\000\048\000\050\000\052\000\053\000\
    \055\000\018\000\017\000\055\000\058\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\025\000\255\255\255\255\255\255\026\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\255\255\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\255\255\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 13 "src/FbSR/fbsrlexer.mll"
    (token lexbuf)
# 152 "src/FbSR/fbsrlexer.ml"

  | 1 ->
# 14 "src/FbSR/fbsrlexer.mll"
                       ( token lexbuf )
# 157 "src/FbSR/fbsrlexer.ml"

  | 2 ->
# 15 "src/FbSR/fbsrlexer.mll"
                       ( AND )
# 162 "src/FbSR/fbsrlexer.ml"

  | 3 ->
# 16 "src/FbSR/fbsrlexer.mll"
                       ( OR )
# 167 "src/FbSR/fbsrlexer.ml"

  | 4 ->
# 17 "src/FbSR/fbsrlexer.mll"
                       ( NOT )
# 172 "src/FbSR/fbsrlexer.ml"

  | 5 ->
# 18 "src/FbSR/fbsrlexer.mll"
                       ( FUNCTION )
# 177 "src/FbSR/fbsrlexer.ml"

  | 6 ->
# 19 "src/FbSR/fbsrlexer.mll"
                       ( FUNCTION )
# 182 "src/FbSR/fbsrlexer.ml"

  | 7 ->
# 20 "src/FbSR/fbsrlexer.mll"
                       ( IF )
# 187 "src/FbSR/fbsrlexer.ml"

  | 8 ->
# 21 "src/FbSR/fbsrlexer.mll"
                       ( THEN )
# 192 "src/FbSR/fbsrlexer.ml"

  | 9 ->
# 22 "src/FbSR/fbsrlexer.mll"
                       ( ELSE )
# 197 "src/FbSR/fbsrlexer.ml"

  | 10 ->
# 23 "src/FbSR/fbsrlexer.mll"
                       ( IN )
# 202 "src/FbSR/fbsrlexer.ml"

  | 11 ->
# 24 "src/FbSR/fbsrlexer.mll"
                       ( REF )
# 207 "src/FbSR/fbsrlexer.ml"

  | 12 ->
# 25 "src/FbSR/fbsrlexer.mll"
                       ( LET )
# 212 "src/FbSR/fbsrlexer.ml"

  | 13 ->
# 26 "src/FbSR/fbsrlexer.mll"
                       ( REC )
# 217 "src/FbSR/fbsrlexer.ml"

  | 14 ->
# 27 "src/FbSR/fbsrlexer.mll"
                       ( SET )
# 222 "src/FbSR/fbsrlexer.ml"

  | 15 ->
# 28 "src/FbSR/fbsrlexer.mll"
                       ( GOESTO )
# 227 "src/FbSR/fbsrlexer.ml"

  | 16 ->
# 29 "src/FbSR/fbsrlexer.mll"
                       ( BOOL false )
# 232 "src/FbSR/fbsrlexer.ml"

  | 17 ->
# 30 "src/FbSR/fbsrlexer.mll"
                       ( BOOL true )
# 237 "src/FbSR/fbsrlexer.ml"

  | 18 ->
# 31 "src/FbSR/fbsrlexer.mll"
                       ( EOEX )
# 242 "src/FbSR/fbsrlexer.ml"

  | 19 ->
# 32 "src/FbSR/fbsrlexer.mll"
                       ( PLUS )
# 247 "src/FbSR/fbsrlexer.ml"

  | 20 ->
# 33 "src/FbSR/fbsrlexer.mll"
                       ( MINUS )
# 252 "src/FbSR/fbsrlexer.ml"

  | 21 ->
# 34 "src/FbSR/fbsrlexer.mll"
                       ( EQUAL )
# 257 "src/FbSR/fbsrlexer.ml"

  | 22 ->
# 35 "src/FbSR/fbsrlexer.mll"
                       ( LPAREN )
# 262 "src/FbSR/fbsrlexer.ml"

  | 23 ->
# 36 "src/FbSR/fbsrlexer.mll"
                       ( RPAREN )
# 267 "src/FbSR/fbsrlexer.ml"

  | 24 ->
# 37 "src/FbSR/fbsrlexer.mll"
                       ( LCURLY )
# 272 "src/FbSR/fbsrlexer.ml"

  | 25 ->
# 38 "src/FbSR/fbsrlexer.mll"
                       ( RCURLY )
# 277 "src/FbSR/fbsrlexer.ml"

  | 26 ->
# 39 "src/FbSR/fbsrlexer.mll"
                       ( SEMI )
# 282 "src/FbSR/fbsrlexer.ml"

  | 27 ->
# 40 "src/FbSR/fbsrlexer.mll"
                       ( DOT )
# 287 "src/FbSR/fbsrlexer.ml"

  | 28 ->
# 41 "src/FbSR/fbsrlexer.mll"
                       ( GET )
# 292 "src/FbSR/fbsrlexer.ml"

  | 29 ->
# 42 "src/FbSR/fbsrlexer.mll"
                       ( INT (int_of_string(Lexing.lexeme lexbuf)) )
# 297 "src/FbSR/fbsrlexer.ml"

  | 30 ->
# 43 "src/FbSR/fbsrlexer.mll"
                       ( IDENT (Lexing.lexeme lexbuf) )
# 302 "src/FbSR/fbsrlexer.ml"

  | 31 ->
# 44 "src/FbSR/fbsrlexer.mll"
                       ( raise Exit )
# 307 "src/FbSR/fbsrlexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

