(*  OBJECTIVE:

- dati dellinterprete: expr, env, store, com
- dati del linguaggio: Int, Bool ecc + dati definibili dallutente
- funzioni su dati come tipo?

*)
(* ***************TIPAGGIO E DOMINI (parsing)****************** *)
(*type variable = string
type value = Int of int | Bool of bool*)
type expr =  
(* E::=I | val(I) | lambda(I,E1) | plus(E1,E2) | apply(E1,E2) *)
| Id of string
| Bool of bool
| Int of int
| Plus of expr*expr
| Minus of expr*expr
| Equal of expr*expr
| If of expr*expr*expr
(*| Lambda of string*expression (* lambda id.expr *)
| Apply of expression*expression (* dafuq is this? *) (* maybe to the left u have a function and to the right the actual args "f args" *)
*)

type env = (string -> expr) (* exprr??? it should be a value *)
(* type env = (variable->value) *)
(*type command = 
| Assign of variable*expression
| Ifthenelse of expression*command list*command list 
| While of expression*command list *)

(* ****************IMPLEMENTAZIONE SEMANTICA*************** *)
(*let typecheck var typename = match typename with
| "Int" -> match var with Int(n)->true | _->false
| "Bool" -> match var with Bool(b)->true | _->false
| _ -> false (* None? *) *)
let plus x y = match x,y with
Int a, Int b -> a+b
_ -> None
let minus x y = match x,y with
Int a, Int b -> a-b
_ -> None
let equal x y = match x,y with (* only int? *)
Int a, Int b -> a=b
_ -> None
let ifthenelse b x y = match b with
true -> x
_ -> y

let sem (e:expr) (r:env) = match e with
| Id i -> r i
| Bool b -> b
| Int n -> n
| Plus e1,e2 -> (sem e1 r) + (sem e2 r)
| Minus e1,e2 -> (sem e1 r) - (sem e2 r)
| Equal e1,e2 -> (sem e1 r) = (sem e2 r)
| If e1,e2,e3 -> if (sem e1 r) then (sem e2 r) else (sem e3 r) (*EMBED FOR PERFORMANCE!*)
