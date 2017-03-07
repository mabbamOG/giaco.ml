(***********************************************)
(*****************SYNTAX DOMAINS****************)
(***********************************************)
type ide = string
type expr =
| Val of evalue | Var of ide
(** iop: *)
| Add of expr*expr | Mul of expr*expr | Sub of expr*expr | Div of expr*expr
(** bop: *)
| Or of expr*expr | Xor of expr*expr | And of expr*expr | Not of expr
(** fun: *)
| Lambda of ide*expr (* function string -> expr *)
| Apply of expr*expr (* funexpr expr *)
| If of expr*expr*expr
(** syntax aliases: *)
let int n= Val(Int(n))
let bool b = Val(Bool(b))
let float f = Val(Float(f))

(***********************************************)
(*****************SEMANTIC DOMAINS**************)
(***********************************************)
type evalue = Int of int | Bool of bool | Float of float | Fun of (ide->evalue)
type env = (ide -> evalue) (* dvalue? *)

(***********************************************)
(*****************SEMANTIC EVALUATION***********)
(***********************************************)

(* evalue -> evalue functions*)
;;#use "iop.ml"
;;#use "bop.ml"

(* interpreter for the language, environment is static and global *)
(** type is env -> expr -> value *)
let rec evaluate env = function
| Val v -> v
| Var x -> env x
(** iop *)
| Add (e1,e2)-> add (eval env e1,eval env e2)
| Sub (e1,e2)-> sub (eval env e1, eval env e2)
| Mul (e1,e2)-> mul (eval env e1, eval env e2)
| Div (e1,e2)-> div (eval env e1, eval env e2)
(** bop *)
| And (e1,e2)-> _and (eval env e1, eval env e2)
| Or (e1,e2)-> _or (eval env e1, eval env e2)
| Xor (e1,e2)-> xor (eval env e1, eval env e2)
| Not (e)-> _not (eval env e)
(** functions *)
| Lambda (x,e) ->  Fun (function d -> evaluate (function x->d | other -> env other) e)
| Apply (f,e) -> (evaluate env f) (evaluate env e)
| If (c,e1,e2) -> if (evaluate env c)=Bool(true) then (evaluate env e1) else (evaluate env e2)


(***********************************************)
(*****************TESTING***********************)
(***********************************************)
;;print_string "---------------commence phase of testing----------------"

let test_env = function
| "a" -> Int 3
| "b" -> Int 4
| "ciao" -> Int 42
| _ -> failwith "variable not bound"

;;eval test_env (int 5)
;;eval test_env (bool true)
;;eval test_env (Add(int 5, int 5))
;;print_string "---------------end phase of testing----------------"

(***********************************************)
(*****COMMANDDDDDDREFLECTION********************)
(***********************************************)
(*type typo = RInt of string*Int | *)
(* 1. lex string into list of tokens *)
(* 2. parse commands by splitting @ ; (?) and then read first word *)
(* 3. profit *)
(* 4. ???? *)
