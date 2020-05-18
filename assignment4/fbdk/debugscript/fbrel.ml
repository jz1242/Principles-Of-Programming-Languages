(*
   File: fbrel.ml
   Authors: Scott Smith, Zachary Palmer

   This file allows interactive testing of your FbReL interpreter using the FbDK.  See
   the README.md file in the FbDK for usage instructions.
*)


(* First load all the relevant compiled structs.  *)
#directory "_build/src";; 
#directory "_build/src/FbreL";; (* change to "binaries/libraries" to run solution interpreters *)
#load "fbdk.cmo";;
#load "fbrelast.cmo";;
#load "fbrelparser.cmo";;
#load "fbrellexer.cmo";;
#load "fbrelpp.cmo";;
#load "fbrelinterp.cmo";;
(* for typechecker: #load "fbreltype.cmo";; *)

(* Make some structs available at the top for easier use *)

open Fbrelast;;
open Fbrelinterp;;
(* for typechecker: open Fbreltype;; *)

(* parse parses Fbrel concrete syntax you enter as a string *)

let parse s =
    let lexbuf = Lexing.from_string (s^";;") in
  	Fbrelparser.main Fbrellexer.token lexbuf;;

(* unparse is the reverse of parsing: expr to string *)

let unparse e = Fbrelpp.pretty_print e;;

(* pp is a top-loop pretty printer *)

let pp e = print_string (unparse e);;

(* ppeval evaluates an expr and then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for Fbrel programs: an interpreter *)

let rep s = ppeval (parse s);;

(* res is like rep but just returns the string result - no printing it out. *)

let res s = unparse (eval (parse s));;

(* Examples. *)

let code = "(Fun rec -> Fun arg -> 
               If arg = 0 Then 0 Else arg + rec (arg - 1))"

let ycomb = "(Fun code -> Let repl = Fun self -> Fun x -> code (self self) x In repl repl)";;

let goy = ycomb^code^" 5";;

let ex = parse goy;;

let result = eval ex;;

pp result;;

ppeval ex;;

rep goy;;

