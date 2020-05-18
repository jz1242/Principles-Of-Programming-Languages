{
  open Fbrelparser;;
  
  exception StringSyntaxError of string
} 

let blank = [' ' '\t' '\n' '\r']
let decimal_literal = ['0'-'9']+
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
  ['(']['*']([^'*']|['*'][^')'])*['*'][')'] 
    {token lexbuf} (* Ignore comments *)
| blank+               { token lexbuf }
| "And"                { AND }
| "Or"                 { OR }
| "Not"                { NOT }
| "Fun"                { FUNCTION }
| "Function"           { FUNCTION }
| "If"                 { IF }
| "Then"               { THEN }
| "Else"               { ELSE }
| "Let"                { LET }
| "In"                 { IN }
| "->"                 { GOESTO }
| "False"              { BOOL false }
| "True"               { BOOL true }
| ";;"                 { EOEX }
| ";"                  { SEMICOLON }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '='                  { EQUAL }
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '|'                  { PIPE }
| '['                  { LBRACKET }
| ']'                  { RBRACKET }
| "::"                 { CONS }
| "Match"              { MATCH }
| "With"               { WITH }
| '{'                  { LCURLY }
| '}'                  { RCURLY }
| '.'                  { DOT }
| decimal_literal      { INT (int_of_string(Lexing.lexeme lexbuf)) }
| lowercase identchar* { IDENT (Lexing.lexeme lexbuf) }
| eof                  { raise Exit }


{} 



