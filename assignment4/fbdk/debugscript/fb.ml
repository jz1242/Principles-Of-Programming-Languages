(*
   File: fb.ml
   Authors: Scott Smith, Zachary Palmer

   This file allows interactive testing of your interpreter using the FbDK.  See
   the README.md file in the FbDK for usage instructions.
*)


(* First load all the relevant compiled structs.  *)
#directory "binaries/libraries";; 
#directory "binaries/libraries/Fb";;
(* #directory "_build/src";; 
#directory "_build/src/Fb";; change to "binaries/libraries" to run solution interpreters *)
#load "fbdk.cmo";;
#load "fbast.cmo";;
#load "fbparser.cmo";;
#load "fblexer.cmo";;
#load "fbpp.cmo";;
#load "fbinterp.cmo";;
(* for typechecker: #load "fbtype.cmo";; *)

(* Make some structs available at the top for easier use *)

open Fbast;;
open Fbinterp;;
(* for typechecker: open Fbtype;; *)

(* parse parses Fb concrete syntax you enter as a string *)

let parse s =
    let lexbuf = Lexing.from_string (s^";;") in
  	Fbparser.main Fblexer.token lexbuf;;

(* unparse is the reverse of parsing: expr to string *)

let unparse e = Fbpp.pretty_print e;;

(* pp is a top-loop pretty printer *)

let pp e = print_string ((unparse e)^"\n");;

(* pps pretty prints a string program by parsing and unparsing *)

let pps e = pp (parse e);;

(* ppeval evaluates an expr and then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for Fb programs: an interpreter *)

let rep s = ppeval (parse s);;

(* res is like rep but just returns the string result - no printing it out. *)

let res s = unparse (eval (parse s));;

(* Examples. *)

let s1 =
  "Let Rec x1 x2 =
     If x2 = 1 Then
        (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
        x1 (x2 - 1)
   In x1 100";;

let ex1 = parse s1;;

let result1 = eval ex1;;

pp result1;;

ppeval ex1;;

rep s1;;

(* For typechecking when we get to that in class:

typecheck ex1;; *)
