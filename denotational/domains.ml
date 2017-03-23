(* DOMAINS - SEMANTIC - VALUES *)
type evalue = Int of int | Bool of bool | Float of float | Fun of (ide->evalue)

(* DOMAINS - SYNTAX *)
type ide = string
type expr =
| Val of evalue | Var of ide (* U WOT MATE? *)
| Equals of expr*expr (* POLYMORPHIC *)
| Add of expr*expr | Mul of expr*expr | Sub of expr*expr | Div of expr*expr (* NUMBERS *)
| Or of expr*expr | Xor of expr*expr | And of expr*expr | Not of expr (* BOOLEANS *)
| Lambda of ide*expr (* function string -> expr *) | Apply of expr*expr (* funexpr expr *) | If of expr*expr*expr (* FUNCTIONS *)
(** syntax helpers *)
let int n= Val(Int(n))
let bool b = Val(Bool(b))
let float f = Val(Float(f))

(* DOMAINS - SEMANTIC - STATES *)
type env = (ide -> evalue) (* dvalue? *)
