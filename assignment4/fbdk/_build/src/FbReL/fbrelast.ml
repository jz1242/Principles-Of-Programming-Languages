type label = Lab of string

type ident = Ident of string

type expr = 
 Var of ident | Function of ident * expr | Appl of expr * expr |
 Plus of expr * expr | Minus of expr * expr | Equal of expr * expr | 
 And of expr * expr| Or of expr * expr | Not of expr |  
 If of expr * expr * expr | Int of int | Bool of bool | String of string |
 Let of ident * expr * expr | 
 Record of (label * expr) list | Select of  label * expr | 
 Match of expr * expr * ident * ident * expr |
 EmptyList | Cons of expr * expr

type fbtype = TInt | TBool | TArrow of fbtype * fbtype | TVar of string;;

