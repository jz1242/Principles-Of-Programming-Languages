(*
   File: bool.ml
   Authors: Scott Smith, Zachary Palmer

   This file allows interactive use of the BOOL interpreter using the FbDK.  See
   the README.md file in the FbDK for usage instructions.
*)


(* First load all the relevant compiled structs.  *)
#directory "_build/src";;
#directory "_build/src/BOOL";; (* change to "binaries/libraries" to run solution interpreters *)
#load "fbdk.cmo";;
#load "boolast.cmo";;
#load "boolparser.cmo";;
#load "boollexer.cmo";;
#load "boolpp.cmo";;
#load "boolinterp.cmo";;
(* for typechecker: #load "fbtype.cmo";; *)

(* Make some structs available at the top for easier use *)

open  Boolast;;
open Boolinterp;;
(* for typechecker: open Booltype;; *)

(* parse parses Bool concrete syntax you enter as a string *)

let parse s =
    let lexbuf = Lexing.from_string (s^";;") in
  	Boolparser.main Boollexer.token lexbuf;;

(* unparse is the reverse of parsing: expr to string *)

let unparse e = Boolpp.pretty_print e;;

(* pp is a top-loop pretty printer *)

let pp e = print_string (unparse e);;

(* ppeval evaluates an expr and then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for Bool programs: an interpreter *)

let rep s = ppeval (parse s);;

(* res is like rep but just returns the string result - no printing it out. *)

let res s = unparse (eval (parse s));;

(* Examples. *)

let s1 =
  "True And (Not False)";;

let ex1 = parse s1;;

let result1 = eval ex1;;

pp result1;;

ppeval ex1;;

rep s1;;
