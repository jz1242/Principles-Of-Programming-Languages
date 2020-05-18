(*
   File: fbrx.ml
   Authors: Scott Smith, Zachary Palmer, Shiwei Weng

   This file allows interactive testing of your interpreter using the FbDK.  See
   the README.md file in the FbDK for usage instructions.
*)


(* First load all the relevant compiled structs.  *)
#directory "_build/src";;
#directory "_build/src/FbRX";;
#load "fbdk.cmo";;
#load "fbrxast.cmo";;
#load "fbrxparser.cmo";;
#load "fbrxlexer.cmo";;
#load "fbrxpp.cmo";;
#load "fbrxinterp.cmo";;

(* Make some structs available at the top for easier use *)

open Fbrxast;;
open Fbrxinterp;;


(* function parse parses FbSR concrete syntax you enter as a string *)

let parse s =
  let lexbuf = Lexing.from_string (s^";;") in
  Fbrxparser.main Fbrxlexer.token lexbuf;;

(* Function pp is a top-loop pretty printer using FbDK's pretty printer *)

let pp e = print_string (Fbrxpp.pretty_print e);;

(* ppeval evals then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for FbSR programs *)

let rep s = ppeval (parse s);;

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
(* 
let t = "1";;
parse t;;
rep t;;

let t = "True";;
parse t;;
rep t;;

let t = "1+1";;
parse t;;
rep t;;

let t = "1-1";;
parse t;;
rep t;;

let t = "1=1";;
parse t;;
rep t;;

let t = "True And False";;
parse t;;
rep t;;

let t = "False Or False";;
parse t;;
rep t;;

let t = "If True Then 1 Else 0";;
parse t;;
rep t;; *)
(* 
let t = "Raise #Foo 0";;
parse t;;
rep t;;

let t = "Try 1 With #Foo x -> x + 100";;
parse t;;
rep t;;

let t = "Try Raise #Foo 0 With #Foo x -> x + 100";;
parse t;;
rep t;;

let t = "{}";;
parse t;;
rep t;;

let t = "{a = 1}";;
parse t;;
rep t;;

let t = "{a = 1} @ {}";;
parse t;;
rep t;;

let t = "{} @ {a = 1}";;
parse t;;
rep t;;

let t = "{a = 1} @ {b = 2}";;
parse t;;
rep t;;

let t = "{a = 1} @ {a = 2}";;
parse t;;
rep t;;
 *)
(* 
let t = "1 + Raise #Foo 0";;
parse t;;
rep t;;

let t = "True Or Raise #Foo 0";;
parse t;;
rep t;;

let t = "False Or Raise #Foo 0";;
parse t;;
rep t;;

let t = "Raise #Foo 0 And False";;
parse t;;
rep t;;

let t = "If True Then 1 Else Raise #Foo 0";;
parse t;;
rep t;;

let t = "If False Then 1 Else Raise #Foo 0";;
parse t;;
rep t;;

let t = "Raise #Foo (Raise #Bar (Raise #Baz 2333))";;
parse t;;
rep t;; *)

let t = "Let x = Raise #Foo 0 In x";;
parse t;;
rep t;;

let t = "Let Rec f x = Raise #Foo 0 In f (Raise #Bar 1)";;
parse t;;
rep t;;

let t = "Let x = Raise #Foo 0 In Let y = Raise #Bar 1 In Raise #Baz 2";;
parse t;;
rep t;;


let t = "Fun x -> Raise #Foo 0";;
parse t;;
rep t;;

let t = "(Fun x -> Raise #Foo 0) (Raise #Bar 1)";;
parse t;;
rep t;;

let t = "{} @ (Raise #Foo 0)";;
parse t;;
rep t;;

let t = "{a = 1; b = \"good good\"}.b";;
parse t;;
rep t;;

let t = "{a = 1; b = {b = \"good good\"}}.b.b";;
parse t;;
rep t;;

(* let t = "{a = 1; b = {b = \"good good\"}}.b.a";;
   parse t;;
   rep t;; *)

let t = "{a = 1; b = 2} = {b = 2; a = 1}";;
parse t;;
rep t;;