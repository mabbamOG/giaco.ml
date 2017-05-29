;;#use "domains.ml"

(* INTERPRETATION - EVALUATION *)
(* evalue -> evalue functions*)
;;#use "numbers.ml"
;;#use "booleans.ml"
let equals = function
| Int _,Int _ | Bool _,Bool _ | Float _,Float _ -> Bool(true)
| Fun _, Fun _ -> failwith "im sorry, i don't know how to compare functions"
| _ -> failwith "i'm sorry, these values are incompatible!"

(* interpreter for the language, environment is static and global *)
(** type is env -> expr -> value *)
let rec evaluate env = function
| Val v -> v
| Var x -> env x
(** gen op *)
| Equals (e1,e2) -> equals (evaluate env e1, evaluate env e2)
(** iop *)
| Add (e1,e2)-> add (evaluate env e1,evaluate env e2)
| Sub (e1,e2)-> sub (evaluate env e1, evaluate env e2)
| Mul (e1,e2)-> mul (evaluate env e1, evaluate env e2)
| Div (e1,e2)-> div (evaluate env e1, evaluate env e2)
(** bop *)
| And (e1,e2)-> _and (evaluate env e1, evaluate env e2)
| Or (e1,e2)-> _or (evaluate env e1, evaluate env e2)
| Xor (e1,e2)-> xor (evaluate env e1, evaluate env e2)
| Not (e)-> _not (evaluate env e)
(** functions *)
| Lambda (x,e) ->  Fun (function d -> evaluate (function x->d | other -> env other) e)
| Apply (f,e) -> (evaluate env f) (evaluate env e)
| If (c,e1,e2) -> match (evaluate env c) with Bool b -> if b=true then (evaluate env e1) else (evaluate env e2) | _ -> failwith "check must be of bool type"


(***********************************************)
(*****************TESTING***********************)
(***********************************************)
;;print_string "---------------commence phase of testing----------------"
let test_env = function
| "a" -> Int 3
| "b" -> Int 4
| "ciao" -> Int 42
| _ -> failwith "variable not bound"
;;evaluate test_env (int 5)
;;evaluate test_env (bool true)
;;evaluate test_env (Add(int 5, int 5))
;;evaluate test_env (Equals(Mul(int 3,int 3),int 9))
;;evaluate test_env (IfThenElse(Equals(Mul(Var "a",int 3),int 9)),Var "ciao",Var "b")
;;print_string "---------------end phase of testing----------------"

(***********************************************)
(*****COMMANDDDDDDREFLECTION********************)
(***********************************************)
(*type typo = RInt of string*Int | *)
(* 1. lex string into list of tokens *)
(* 2. parse commands by splitting @ ; (?) and then read first word *)
(* 3. profit *)
(* 4. ???? *)
