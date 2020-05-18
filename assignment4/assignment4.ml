(*

PoPL Assignment 4 part 2
Your Name : Jason Zhang
List of Collaborators : Dan Qian

   For this part, you will write some programs in Fb.  Your answers for
   this section must be in the form of OCaml strings which the parse
   function in debugscript/fb.ml will successfully turn into Fb ASTs.
   If you want a macro for repeated code, you are wecome to use OCaml
   to put strings together.  Here is an example from debugscript/fb_examples.ml:

 *)

let pair c1 c2  = "((Fun lft -> Fun rgt -> Fun x -> x lft rgt) ("^c1^") ("^c2^"))";;
let left c =  "(("^c^") (Fun x -> Fun y -> x))";;
let right c =  "(("^c^") (Fun x -> Fun y -> y))";;

let project_my_pair = left (pair "34" "45");;

(* 

Here is a test of the above; it assumes you input the debugscript file previously:
# #use "debugscript/fb.ml";; (* launch ocaml from fbdk directory for this to work *)
...
# res project_my_pair 
- : string = "34"

*)

(*
Do realize this is a VERY primitive macro system, you will want to put () around
any definition you make or when appended the parse order could change.

For questions in this section you are not allowed to use the Let Rec syntax even
if you have implemented it in your interpreter. Any recursion that you use must
entirely be in terms of Functions. Feel free to implement an Fb Y-combinator
here.  For examples and hints, see the file "fbdk/debugscript/fb_examples.ml".

Remember to test your code against the standard Fb binaries (and not just your own 
implementation of Fb) to ensure that your functions work correctly.

*)

(* Part 2 question 1.

   Fb fails to provide any operations over integers more complex
   than addition and subtraction.  Below, define the following
   2-argument Fb functions: less-than, multiplication, and mod, the
   modulus operator.  (Hint: if you get stuck, try getting them
   working for positive numbers first and then dealing with
   negatives.)  *)
let ycomb = "(Fun code -> Let repl = Fun self -> Fun x -> code (self self) x In repl repl)";;

let joeY = "(Fun code -> Let repl = Fun this -> Fun arg -> code arg (this this) In repl repl)";;

let joeFix = "(Fun code -> Fun this-> Fun arg -> code arg this)";;

let joeYY = "(Fun code -> code code)";;

let mess = "(Function this -> Function arg ->
           If arg = 0 Then 0 Else arg + this this (arg - 1))";;
let code = "(Function arg -> Function this -> 
               If arg = 0 Then 
                  0
               Else 
                  arg + this (arg - 1))";;
let code = "(Function this -> Function arg -> 
               If arg = 0 Then 
                  0
               Else 
                  arg + this (arg - 1))";;

let code = "Fun x -> Fun y -> x";;
let test = ycomb^joeFix^code^" 5";;
let test2 = joeYY^mess^" 5";;

let codeLt = "(Fun rec -> Fun neg -> Fun pos -> Fun matchArg -> Fun matchArg2 -> Fun arg -> Fun arg2 ->
               If arg = neg And matchArg2 Then
                  True
               Else
                  If arg = neg And Not(matchArg2) Then
                     rec (neg - 1) (pos + 1) True False arg arg2
                  Else
                     If arg2 = neg And matchArg Then
                        False
                     Else
                        If arg2 = neg And Not(matchArg) Then
                           rec (neg - 1) (pos + 1) False True arg arg2
                        Else
                           If arg = pos And matchArg2 Then
                              False
                           Else
                              If arg = pos And Not(matchArg2) Then
                                 rec (neg - 1) (pos + 1) True False arg arg2
                              Else
                                 If arg2 = pos And matchArg Then
                                    True
                                 Else
                                    If arg2 = pos And Not(matchArg) Then
                                       rec (neg - 1) (pos + 1) False True arg arg2
                                    Else
                                       If arg = arg2 Then
                                          False
                                       Else
                                          rec (neg - 1) (pos + 1) matchArg matchArg2 arg arg2)";;

let fbLt = ycomb^codeLt^" 0 0 False False";;

let checkNegativeFun = "(Fun checkNeg -> Fun countPos -> Fun countNeg -> Fun arg ->
                           If arg = countPos Then
                              False
                           Else
                              If arg = countNeg Then
                                 True
                              Else
                                 checkNeg (countPos + 1) (countNeg - 1) arg)";;

let checkNeg = ycomb^checkNegativeFun^" 0 0";;


let codeMult = "(Fun multi -> Fun arg -> Fun arg2 ->
                  If arg = 0 Or arg2 = 0 Then
                     0
                  Else 
                     If ("^checkNeg^" arg2) Then
                        0 - arg + multi arg (arg2 + 1)
                     Else
                        arg + multi arg (arg2 - 1)
                )";;
let fbMult = ycomb^codeMult;;


let codeMod = "(Fun mod -> Fun dividend -> Fun divisor ->
                  If divisor = 0 Then
                     (0 1)
                  Else 
                     If ("^checkNeg^" (dividend - divisor))Then
                        dividend
                     Else
                        If dividend = 0 Then
                           0
                        Else
                           mod (dividend - divisor) divisor
                )";;

let fbMod = ycomb^codeMod;;
  
(*

assert(res ("("^fbLt^") 33 3") = "False");;
assert(res ("("^fbLt^") (0-1) 3") = "True");;
assert(res ("("^fbMult^") 5 3") = "15");;
assert(res ("("^fbMult^") (0-3) 5") = "-15");;
assert(res ("("^fbMod^") 33 3") = "0");;
assert(res ("("^fbMod^") 87 4") = "3");;

*)

(*  Part 2 question 2.


*)


(*

   Fb is a simple language. But even it contains more constructs than strictly necessary.
   For example, you don't even need integers! They can be encoded using just
   functions using what is called Church's encoding http://en.wikipedia.org/wiki/Church_encoding

   Essentially this encoding allows us to represent integers as functions. For example:

        0 --> Function f -> Function x -> x
        1 --> Function f -> Function x -> f x
        2 --> Function f -> Function x -> f (f x)

   We will write 4 functions that work with church numerals in this section. Remember that all
   your answers should generate Fb ASTs.

   You can assume that we are dealing with only non-negative integers in this question.

*)


(* Write a Fb function to convert a church encoded value to an Fb native integer.*)
let increment = "(Fun x -> x + 1)";;
let combUnchurch = "(Fun f1 -> Fun f2 ->
                     f2 f1 0)";;
let fbUnchurch = combUnchurch^increment;;


(* Write a Fb function to convert an Fb native integer to a church encoded value *)
let fun0 = "(Function f -> Function x -> x)";;
let funG = "(Function n -> Function f -> Function x -> f (n f x))";;

let combChurch = "(Fun enc -> Fun prev -> Fun count -> Fun f -> Fun x ->
                     If count = 0 Then
                        "^fun0^" f x
                     Else
                        f (enc ("^funG^" prev) (count - 1) f x)

                  )";; 
let fbChurch = ycomb^combChurch^fun0;;


(*
let church2 = "Function f -> Function x -> f (f x)";;
assert ( res ("("^fbUnchurch^")("^church2^")") = "2" );;
assert ( res ("("^fbChurch^" 4) (Function n -> n + n) 3") = "48" );;
*)

(* Write a function to add two church encoded values *)
let funAdd = "(Function m -> Function n -> Function f -> Function x -> (m f) ((n f) x))";;
let fbChurchAdd = funAdd;;

(* Write a function to multiply two church encoded values *)
let funMult = "(Function m -> Function n -> Function f -> Function x -> (m (n f) x))";;
let fbChurchMult = funMult;;

(*
let church2 = "(Function f -> Function x -> f (f x))";;
let church3 =  "(Function f -> Function x -> f (f (f x)))" ;;
assert ( res (fbUnchurch^"("^fbChurchAdd^church3^church2^")") = "5" );;
assert ( res (fbUnchurch^"("^fbChurchMult^church3^church2^")") = "6" );;
*)
